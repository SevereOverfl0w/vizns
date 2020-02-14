(ns io.dominic.vizns.core
  (:require
    [clojure.tools.cli :as cli]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.deps.alpha :as tools.deps]
    [clojure.tools.deps.alpha.reader :as tools.deps.reader]
    [clojure.tools.deps.alpha.util.dir :as tools.deps.util.dir]
    [clojure.java.io :as io]
    [clj-kondo.core :as kondo]
    [dorothy.core :as dot]
    [dorothy.jvm :as dot.jvm])
  (:import [java.util.jar JarFile JarFile$JarFileEntry]))

(defn- analyze
  [paths]
  (:analysis (kondo/run!
               {:lint paths
                :config {:output {:analysis true}}})))

(defn- project-nss
  [analysis]
  (set (map :name (:namespace-definitions analysis))))

(defn- ns-relations
  [analysis]
  (let [project-nss (project-nss analysis)]
  (remove #(contains? project-nss (:to %))
          (:namespace-usages analysis))))

(defn- read-deps
  [deps-file]
  (tools.deps.reader/read-deps
    [(tools.deps.reader/user-deps-location) deps-file]))

(defn- lib-map
  [deps-map deps-file]
  (tools.deps.util.dir/with-dir (.getParentFile (io/file deps-file))
    (tools.deps/resolve-deps deps-map {})))

(defn- path->lib
  [lib-map]
  (into {}
        (mapcat
          (fn [[lib-k lib]]
            (map
              (fn [path]
                [(io/file path) lib-k])
              (:paths lib)))
          lib-map)))

(defn- make-classpath
  [lib-map]
  (map io/file (mapcat :paths (vals lib-map))))

(defn- jar-contains?
  [jar file]
  (boolean (.getEntry (JarFile. jar) file)))

(defn- classpath-entry-contains?
  [classpath-entry file]
  (if (and (.isFile classpath-entry)
           (string/ends-with? (.getName classpath-entry)
                              ".jar"))
    (jar-contains? classpath-entry file)
    (.exists (io/file classpath-entry file))))

(defn- ns->paths
  [ns]
  (let [basis (string/escape (str ns)
                             {\. \/
                              \- \_})]
    [(str basis "__init.class")
     ;; TODO: maybe check filetype in case of mismatch?
     (str basis ".clj")
     (str basis ".cljc")
     (str basis ".cljs")
     (str basis ".js")]))

(defn- find-ns-in-cp
  [classpath ns]
  (some identity
        (for [classpath-entry classpath
              path (ns->paths ns)]
          (when (classpath-entry-contains? classpath-entry path)
            classpath-entry))))

(defn- relations
  "Return a collection of vector pairs, from namespace to library used."
  [lib-map paths]
  (distinct
    (let [path->lib (path->lib lib-map)
          classpath (make-classpath lib-map)
          analysis (analyze paths)
          ns-relations (ns-relations analysis)]
      (for [ns-relation ns-relations
            :let [lib (path->lib (find-ns-in-cp classpath (:to ns-relation)))]
            :when lib]
        [(:from ns-relation) lib]))))

(defn- dot-graph
  [relations lib-map deps-map]
  (let [used-deps (set (map second relations))]
    (dot/digraph
      (concat
        [{:overlap "false"
          :layout "neato"}]
        (map
          (fn [lib]
            [(keyword lib)
             (merge
               {:shape :cylinder
                :label (str lib)
                ;; Needed for cylinder shape
                :margin "0.35,0.1"
                :style :filled
                :fillcolor (if (contains? used-deps lib)
                             :green
                             :grey)})])
          (keys (dissoc (:deps deps-map)
                        'org.clojure/clojure)))
        (map
          (fn [lib]
            [(keyword lib)
             {:shape :cylinder
              :label (str lib)
              :margin "0.35,0.1"
              :fillcolor :red
              :style :filled}])
          (set/intersection
            (set/difference
              (set (keys lib-map))
              (set (keys (dissoc (:deps deps-map) 'org.clojure/clojure))))
            used-deps))
        (map
          (fn [[ns lib]]
            [(keyword ns) (keyword lib)])
          relations)))))

(defn -main
  [& args]
  (let [{{:keys [deps-file show? format output]} :options}
        (cli/parse-opts args
                        [["-d" "--deps DEPS-FILE" "Path to deps file"
                          :id :deps-file
                          :default (.getCanonicalFile (io/file "deps.edn"))
                          :parse-fn #(.getCanonicalFile (io/file %))
                          :validate [#(.exists %)]]
                         [nil "--show" "Show diagram then exit"
                          :id :show?
                          :parse-fn #(new Boolean %)
                          :default false]
                         [nil "--format FORMAT" "One of dot,png,svg"
                          :default "dot"
                          :validate [#{"dot" "png" "svg"}]]
                         ["-o" "--output OUTPUT" "Where to output the graph (not compatible with --show).  Use - for STDOUT"
                          :default "deps-graph.dot"]])
        deps-map (read-deps deps-file)
        lib-map (lib-map deps-map deps-file)
        graph (dot-graph
                (relations
                  lib-map
                  (map #(io/file (.getParent deps-file) %)
                       (:paths deps-map)))
                lib-map
                deps-map)
        dot (dot/dot graph)
        output (if (= "-" output)
                 System/out
                 (io/file
                   (cond-> output
                     (not (string/ends-with? output (str "." format)))
                     (str "." format))))]
    (cond
      show?
      (do (dot.jvm/show! dot)
          (System/exit 0))

      (= format "dot")
      (spit (io/output-stream output) dot)

      :else
      (dot.jvm/save! dot output {:format (keyword format)}))))
