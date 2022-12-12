type^ step_fn [n][e] =
  (cost: *[n]i32)
  -> (nodes_start_index: [n]i32)
  -> (nodes_n_edges: [n]i32)
  -> (edges_dest: [e]i32)
  -> (graph_visited: [n]bool)
  -> (graph_mask: *[n]bool)
  -> (updating_graph_mask: *[n]bool)
  -> (*[n]i32, *[n]bool, *[n]bool)

def generic_bfs [n][e]
                (step: step_fn [n][e])
                (nodes_start_index: [n]i32)
                (nodes_n_edges: [n]i32)
                (edges_dest: [e]i32)
                (is_source: []bool): [n]i32 =
    let (graph_mask, graph_visited, cost) = (copy is_source,
                                             copy is_source,
                                             map (\x -> if x then 0 else -1) is_source)
    let (cost,_,_,_,_) =
      loop (cost, graph_mask, graph_visited, updating_graph_mask, continue) =
           (cost, graph_mask, graph_visited, replicate n false, true)
      while continue do
        let (cost', graph_mask', updating_graph_mask') =
          step cost nodes_start_index nodes_n_edges edges_dest
               graph_visited graph_mask updating_graph_mask

        let step2_inds = map2 (\x i -> if x then i else -1) updating_graph_mask' (iota n)

        let graph_visited' =
            scatter graph_visited step2_inds (replicate n true)

        let graph_mask'' =
            scatter graph_mask' step2_inds (replicate n true)

        let updating_graph_mask'' =
            scatter updating_graph_mask' step2_inds (replicate n false)

        let continue_indices = map (\x -> if x>=0 then 0 else -1) step2_inds
        let continue' =
            scatter [false] continue_indices (replicate n true)

        in (cost', graph_mask'', graph_visited', updating_graph_mask'', continue'[0])

    in cost

def step [n][e]
        (cost: *[n]i32)
        (nodes_start_index: [n]i32)
        (nodes_n_edges: [n]i32)
        (edges_dest: [e]i32)
        (graph_visited: [n]bool)
        (graph_mask: *[n]bool)
        (updating_graph_mask: *[n]bool) : (*[n]i32, *[n]bool, *[n]bool) =
  let [n_indices] (active_indices : [n_indices]i64, _) = unzip (filter (.1) (zip (iota n) graph_mask))

  let graph_mask' =
    scatter graph_mask active_indices (map (const false) active_indices)

  -- We calculate the maximum number of edges for a node.  This is necessary,
  -- since the number of edges are irregular, and since we want to construct a
  -- nested array.
  let e_max = i32.maximum nodes_n_edges
  let active_costs = map (\tid -> #[unsafe] cost[tid]) active_indices

  let flat_len = i64.i32 e_max * n_indices
  let changes = map (\ii -> let row = ii / e_max
                            let col = ii % e_max
                            let tid     = #[unsafe] active_indices[row]
                            let n_edges = #[unsafe] nodes_n_edges[tid]
                            in  #[unsafe]
                                if col < n_edges
                                then let start_index = #[unsafe] nodes_start_index[tid]
                                     let edge_index  = col+start_index
                                     let node_id = #[unsafe] edges_dest[edge_index]
                                     in  if !(#[unsafe] graph_visited[node_id])
                                         then (i64.i32 node_id, active_costs[row]+1)
                                         else (-1, -1)
                                else (-1, -1)
                    ) (map i32.i64 (iota flat_len))

  let (changes_node_ids, changes_costs) = unzip(changes)

  let cost' =
      scatter cost changes_node_ids changes_costs

  let updating_graph_mask' =
      scatter updating_graph_mask changes_node_ids (replicate flat_len true)

  in (cost', graph_mask', updating_graph_mask')

def bfs = generic_bfs step
