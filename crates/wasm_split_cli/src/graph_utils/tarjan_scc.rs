use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::dep_graph::DepNode;

pub type SccId = usize;

pub enum SccEvent<Node = DepNode> {
    Next {
        this: SccId,
        out_edges: HashSet<SccId>,
        // a hack: see [wasm-bindgen hack]
        marked_for_main: bool,
    },
    Member {
        node: Node,
    },
}

#[derive(Default)]
pub struct TarjanSccResult<Node = DepNode> {
    // The set of all dep nodes found in the topological sort
    next_sccid: SccId,
    fully_explored: HashMap<Node, SccId>,
    topological_sort: Vec<SccEvent<Node>>,
}

impl<Node> TarjanSccResult<Node> {
    pub fn new() -> Self {
        Self {
            next_sccid: 1, // 0 is reserved for a virtual node connected to all explored root nodes
            fully_explored: HashMap::new(),
            topological_sort: vec![],
        }
    }

    pub fn scc_of(&self, vertex: &Node) -> Option<SccId>
    where
        Node: Eq + Hash,
    {
        self.fully_explored.get(vertex).cloned()
    }

    pub fn explore<Roots>(
        &mut self,
        roots: Roots,
        graph: &Graph<Node>,
        wbg_in_main: &HashSet<Node>,
    ) -> HashSet<SccId>
    where
        Roots: IntoIterator,
        <Roots as IntoIterator>::Item: Borrow<Node>,
        Node: Eq + Hash + Copy,
    {
        let mut state = TarjanState::new(self, graph, wbg_in_main);
        for root in roots {
            state.connect(*root.borrow());
        }
        state.explore_root.out_edges
    }

    pub fn into_topsort(self) -> Vec<SccEvent<Node>> {
        self.topological_sort
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use crate::graph_utils::tarjan_scc::SccEvent;

    use super::TarjanSccResult;

    #[test]
    fn test_small_graph() {
        let mut searcher = TarjanSccResult::new();
        let graph = HashMap::from([
            (0, HashSet::from([1, 2])),
            (1, HashSet::from([2])),
            (2, HashSet::from([1, 5])),
            (3, HashSet::from([5])),
            (5, HashSet::from([6])),
            (6, HashSet::from([7])),
            (7, HashSet::from([5])),
            (99, HashSet::from([100])),
            (100, HashSet::from([99])),
        ]);
        let roots = searcher.explore([0, 3], &graph, &HashSet::new());

        assert_eq!(
            HashSet::from([0, 1, 2, 3, 5, 6, 7]),
            searcher.fully_explored.keys().cloned().collect(),
        );
        let mut scc_graph = HashMap::new();
        let it = searcher.into_topsort();
        let mut it = it.iter();
        'graph: loop {
            let mut component = vec![];
            loop {
                match it.next() {
                    None => break 'graph,
                    Some(&SccEvent::Member { node: _node }) => {
                        component.push(_node);
                    }
                    Some(&SccEvent::Next {
                        this,
                        ref out_edges,
                        ..
                    }) => {
                        component.sort();
                        scc_graph.insert(component, (this, out_edges.clone()));
                        break;
                    }
                }
            }
        }
        let root_scc = scc_graph
            .get(&[0][..])
            .expect("to find a component for [0]");
        let inner_12 = scc_graph
            .get(&[1, 2][..])
            .expect("to find a component for [1, 2]");
        let inner_567 = scc_graph
            .get(&[5, 6, 7][..])
            .expect("to find a component for [5, 6, 7]");
        let inner_3 = scc_graph
            .get(&[3][..])
            .expect("to find a component for [3]");
        assert!(root_scc.1.contains(&inner_12.0));
        assert!(!root_scc.1.contains(&inner_567.0));
        assert!(inner_12.1.contains(&inner_567.0));
        assert!(inner_3.1.contains(&inner_567.0));

        assert_eq!(HashSet::from([root_scc.0, inner_3.0]), roots);
    }
}

enum WorkItem<Node> {
    Explore {
        vertex: Node,
        parent: Option<Node>,
    },
    Pop {
        vertex: Node,
        parent: Option<Node>,
        link: SccId,
    },
}

struct VertexState {
    // Some(lowlink) if the vertex is on the stack, None otherwise
    lowlink: SccId,
    out_edges: HashSet<SccId>,
    marked_for_main: bool,
}

impl VertexState {
    fn new(link: SccId) -> Self {
        Self {
            lowlink: link,
            out_edges: HashSet::new(),
            marked_for_main: false,
        }
    }
}

type Graph<Node> = HashMap<Node, HashSet<Node>>;

struct TarjanState<'r, Node> {
    result: &'r mut TarjanSccResult<Node>,
    graph: &'r Graph<Node>,
    wbg_in_main: &'r HashSet<Node>,

    explore_root: VertexState,
    stack: Vec<Node>,
    vertex_state: HashMap<Node, VertexState>,
    work: Vec<WorkItem<Node>>,
}

impl<'r, Node: Copy + Eq + Hash> TarjanState<'r, Node> {
    fn new(
        result: &'r mut TarjanSccResult<Node>,
        graph: &'r Graph<Node>,
        wbg_in_main: &'r HashSet<Node>,
    ) -> Self {
        Self {
            result,
            graph,
            explore_root: VertexState::new(0),
            wbg_in_main,

            stack: vec![],
            vertex_state: HashMap::new(),
            work: vec![],
        }
    }

    fn should_explore(&self, vertex: &Node) -> bool {
        self.result.scc_of(vertex).is_none() && !self.vertex_state.contains_key(vertex)
    }

    fn connect(&mut self, vertex: Node) {
        if let Some(scc) = self.result.scc_of(&vertex) {
            self.explore_root.out_edges.insert(scc);
            return;
        }
        debug_assert!(self.work.is_empty() && self.vertex_state.is_empty());
        self.work.push(WorkItem::Explore {
            vertex,
            parent: None,
        });
        self.drain_work();
    }

    fn drain_work(&mut self) {
        while let Some(work) = self.work.pop() {
            match work {
                WorkItem::Explore { vertex, parent } => {
                    if !self.should_explore(&vertex) {
                        // Double check, because we push all neighbors before exploring any.
                        // In the graph [(a, b), (b, c), (a, c)], `c` might otherwise get explored twice.
                        continue;
                    }
                    self.explore(vertex, parent);
                }
                WorkItem::Pop {
                    vertex,
                    parent,
                    link,
                } => {
                    self.pop(vertex, parent, link);
                }
            }
        }
    }

    fn explore(&mut self, vertex: Node, parent: Option<Node>) {
        let scc = self.result.next_sccid;
        self.result.next_sccid += 1;
        self.work.push(WorkItem::Pop {
            vertex,
            parent,
            link: scc,
        });
        let mut v_state = VertexState {
            lowlink: scc,
            out_edges: HashSet::new(),
            marked_for_main: false,
        };
        let v_lowlink = &mut v_state.lowlink;
        self.stack.push(vertex);
        let neighbors = self.graph.get(&vertex);
        if let Some(deps) = neighbors {
            if !self.wbg_in_main.is_disjoint(deps) {
                v_state.marked_for_main = true;
            }
        }
        for &neighbor in neighbors.into_iter().flatten() {
            if let Some(&VertexState { lowlink, .. }) = self.vertex_state.get(&neighbor) {
                *v_lowlink = lowlink.min(*v_lowlink);
            } else if let Some(to_scc) = self.result.scc_of(&neighbor) {
                v_state.out_edges.insert(to_scc);
            } else {
                self.work.push(WorkItem::Explore {
                    vertex: neighbor,
                    parent: Some(vertex),
                });
            }
        }
        let _old = self.vertex_state.insert(vertex, v_state);
        debug_assert!(_old.is_none());
    }

    fn pop(&mut self, vertex: Node, parent: Option<Node>, link: SccId) {
        // still have state, still on the stack here
        let lowlink = self.vertex_state.get_mut(&vertex).unwrap().lowlink;
        let defines_new_scc = link == lowlink;
        if defines_new_scc {
            // form a new scc
            let mut out_edges = HashSet::new();
            let mut marked_for_main = false;
            while let Some(w) = self.stack.pop() {
                let stack_state = self.vertex_state.remove(&w).unwrap();
                out_edges.extend(stack_state.out_edges);
                marked_for_main |= stack_state.marked_for_main;

                self.result
                    .topological_sort
                    .push(SccEvent::Member { node: w });
                self.result.fully_explored.insert(w, lowlink);
                if w == vertex {
                    break;
                }
            }
            self.result.topological_sort.push(SccEvent::Next {
                this: lowlink,
                out_edges,
                marked_for_main,
            });
        }
        // propagate lowlink
        let par_state = if let Some(parent) = parent {
            self.vertex_state.get_mut(&parent).unwrap()
        } else {
            &mut self.explore_root
        };
        if defines_new_scc {
            par_state.out_edges.insert(lowlink);
            debug_assert!(par_state.lowlink < lowlink);
        } else {
            par_state.lowlink = lowlink.min(par_state.lowlink);
        }
    }
}
