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

(defn- make-lib-info
  [relations lib-map deps-map]
  (let [used-deps (set (map second relations))]
    (zipmap
      (keys lib-map)
      (for [[lib data] lib-map]
        (merge data
               {::used? (contains? used-deps lib)
                ::transitive? (not (contains? (:deps deps-map) lib))})))))

(defn- escape-fname
  [sym]
  (string/escape (str (if (= (namespace sym) (name sym))
                        (name sym)
                        sym))
                 {\/ "__"}))

(defn- make-lib-node
  [lib resolved-dep+ opts]
  (let [{:keys [url?]} opts]
    [(keyword lib)
     (merge
       {:shape :cylinder
        :label (str lib)
        ;; Needed for cylinder shape
        :margin "0.35,0.1"
        :style :filled
        :fillcolor (cond
                     (and
                       (::transitive? resolved-dep+)
                       (::used? resolved-dep+)) :red

                     (::used? resolved-dep+) :green

                     (not (::transitive? resolved-dep+)) :grey
                     
                     (::transitive? resolved-dep+) :blue)}
       (when url?
         {:URL (str "./" (escape-fname lib) ".svg")}))]))

(defn- make-ns-node
  [ns opts]
  (let [{:keys [url?]} opts]
    [(keyword ns)
     (merge
       {:label (str ns)}
       (when url?
         {:URL (str "./" (escape-fname ns) ".svg")}))]))

(defn- dot-graph
  [relations nodes]
  (dot/digraph
    (concat
      [{:overlap "false"
        :layout "neato"}]
      nodes
      (map
        (fn [[ns lib]]
          [(keyword ns) (keyword lib)])
        relations))))

(defn- existing-parent-files [parent-dir paths]
  (filter #(.exists (io/file parent-dir %)) paths))

(defn- single
  [common-opts single-opts]
  (let [{:keys [deps-file]} common-opts
        {:keys [show? format output]} single-opts
        deps-map (read-deps deps-file)
        lib-map (lib-map deps-map deps-file)
        relations (relations
                    lib-map
                    (existing-parent-files (.getParentFile deps-file) (:paths deps-map)))
        lib-info (make-lib-info relations lib-map deps-map)
        lib-nodes (zipmap (keys lib-map)
                          (for [[lib resolved-dep+] lib-info]
                            (make-lib-node
                              lib resolved-dep+
                              {:url? true})))
        ns-nodes (zipmap (map first relations)
                         (map #(make-ns-node % {:url? true})
                              (map first relations)))
        dot (dot/dot
              (dot-graph relations
                         (concat (vals (select-keys
                                         lib-nodes
                                         (map second relations)))
                                 (vals ns-nodes))))
        output (if (= "-" output)
                 System/out
                 (io/file
                   (cond-> output
                     (not (string/ends-with? output (str "." format)))
                     (str "." format))))]
    (cond
      show?
      (do
        (dot.jvm/show! dot)
        (println "Hit Ctrl-C to exit"))

      (= format "dot")
      (spit (io/output-stream output) dot)

      :else
      (dot.jvm/save! dot output {:format (keyword format)}))))

(defn- navigate
  [common-opts navigate-opts]
  (let [{:keys [deps-file]} common-opts
        {:keys [output]} navigate-opts
        deps-map (read-deps deps-file)
        lib-map (lib-map deps-map deps-file)
        relations (relations
                    lib-map
                    (existing-parent-files (.getParentFile deps-file) (:paths deps-map)))
        lib-info (make-lib-info relations lib-map deps-map)
        lib-nodes (zipmap (keys lib-map)
                          (for [[lib resolved-dep+] lib-info]
                            (make-lib-node
                              lib resolved-dep+
                              {:url? true})))
        ns-nodes (zipmap (map first relations)
                         (map #(make-ns-node % {:url? true})
                              (map first relations)))]
    (dot.jvm/save! (dot/dot
                     (dot-graph relations
                                (concat (vals (select-keys
                                                lib-nodes
                                                (map second relations)))
                                        (vals ns-nodes))))
                   (doto (io/file output "root.svg")
                     io/make-parents)
                   {:format :svg})
    (doseq [[lib relations] (group-by second relations)]
      (dot.jvm/save! (dot/dot
                       (dot-graph relations
                                  (cons (get lib-nodes lib)
                                        (vals (select-keys ns-nodes (map first relations))))))
                     (io/file output (str (escape-fname lib) ".svg"))
                     {:format :svg}))
    (doseq [[ns relations] (group-by first relations)]
      (dot.jvm/save!
        (dot/dot
          (dot-graph relations
                     (cons (get ns-nodes ns)
                           (vals (select-keys lib-nodes (map second relations))))))
        (io/file output (str (escape-fname ns) ".svg"))
        {:format :svg}))))

(defn -main
  [& args]
  (let [{common-opts :options
         arguments :arguments}
        (cli/parse-opts
          args
          [["-d" "--deps DEPS-FILE" "Path to deps file"
            :id :deps-file
            :default (.getCanonicalFile (io/file "deps.edn"))
            :parse-fn #(.getCanonicalFile (io/file %))
            :validate [#(.exists %)]]]
          :in-order true)
        {command-opts :options}
        (cli/parse-opts
          (rest arguments)
          (case (first arguments)
            "single"
            [[nil "--show" "Show diagram then exit"
              :id :show?
              :parse-fn #(new Boolean %)
              :default false]
             ["-f" "--format FORMAT" "One of dot,png,svg"
              :default "dot"
              :validate [#{"dot" "png" "svg"}]]
             ["-o" "--output OUTPUT" "Where to output the graph (not compatible with --show).  Use - for STDOUT"
              :default "deps-graph.dot"]]

            "navigate"
            [["-o" "--output OUTPUT" "Folder to output the graph to.  Will be created if does not exist."
              :parse-fn io/file
              :default "deps-graph/"]]))]
    ((case (first arguments)
       "single" single
       "navigate" navigate)
     common-opts command-opts)))
