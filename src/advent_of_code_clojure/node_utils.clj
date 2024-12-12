(ns advent-of-code-clojure.node-utils)

(defn nodes-join
  "Recursively joins together nodes into a group of nodes, starting from a collection of nodes.
     
   Input:
     :node           - a single instance of a node in a network.
     :in-group?      - single argument fn (node -> bool) which returns a boolean of whether the node is in the results set.
     :get-neighbours - single argument fn (node -> [nodes]) which returns all neighbours for a node.
     
   Loop parameters:
     :nodes          - the remaining, unprocessed nodes
     :results        - a set containing all nodes that satisfy in-group? and are connected by n get-neighbours links of valid nodes.
     :seen-nodes     - a set containing all nodes seen during processing, regardless of their presence in the result set."
  [nodes in-group? get-neighbours]
  (loop [remaining-nodes nodes results (set nodes)]
    (if (empty? remaining-nodes)
      results

      (let [expanded-new-neighbours (transduce (comp (mapcat get-neighbours) (distinct) (remove results) (filter in-group?)) conj [] remaining-nodes)]
        (recur expanded-new-neighbours (into results expanded-new-neighbours))))))

(defn node-join
  "Recursively joins together nodes into a group of nodes, starting from a single node.
     
   Input:
     :node           - a single instance of a node in a network.
     :in-group?      - single argument fn (node -> bool) which returns a boolean of whether the node is in the results set.
     :get-neighbours - single argument fn (node -> [nodes]) which returns all neighbours for a node."
  [node in-group? get-neighbours]
  (nodes-join (list node) in-group? get-neighbours))

(defn node-join-many
  "Recursively joins together linked nodes in a network into one or more groups of nodes,
   provided the in-group? predicate returns true.
       
     Input:
       :node               - a single instance of a node in a network.
       :create-in-group-fn - single argument fn (node -> fn) which takes the starting node of the group and returns 
                             a fn (node -> bool) that returns a boolean of whether the next node should be in the result set.
       :get-neighbours     - single argument fn (node -> [nodes]) which returns all neighbours for a node."
  [nodes create-in-group-fn get-neighbours]
  (loop [[next-node & rest-nodes] nodes results []]
    (if (nil? next-node)
      results
      (let [next-node-results (node-join next-node (create-in-group-fn next-node) get-neighbours)
            remaining-nodes (->> rest-nodes (remove next-node-results) (into []))]
        (recur remaining-nodes (conj results next-node-results))))))

(defn nodes-join-all
  "Recursively joins together all nodes into one or more groups of nodes.
       
     Input:
       :nodes               - a collection of nodes known to be a part of one of more networks.
       :get-neighbours      - single argument fn (node -> [nodes]) which returns all neighbours for a node."
  [nodes get-neighbours]
  (loop [remaining-nodes (set nodes) results []]
    (if (empty? remaining-nodes)
      results
      (let [next-node (first remaining-nodes)
            next-node-results (node-join next-node remaining-nodes get-neighbours)
            remaining-nodes (apply disj remaining-nodes next-node-results)]
        (recur remaining-nodes (conj results next-node-results))))))