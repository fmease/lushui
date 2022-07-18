#![allow(clippy::implicit_hasher)] // not a priority right now

use super::{HashMap, HashSet};
use std::{collections::hash_map::Entry, hash::Hash};

// @Task add tests!
pub fn find_cycles_by_key<T, U>(
    graph: &HashMap<T, U>,
    get_key: impl Fn(&U) -> &T,
) -> Vec<Cycle<'_, U>>
where
    T: Eq + Hash,
    U: Eq + Hash,
{
    let mut cycles = Vec::new();
    let mut visited = HashMap::default();

    for node in graph.keys() {
        if let Entry::Vacant(entry) = visited.entry(node) {
            entry.insert(Status::Ongoing);
            cycles.extend(find_cycle_by_key(
                graph,
                &get_key,
                &mut vec![node],
                &mut visited,
            ));
        }
    }

    cycles
}

fn find_cycle_by_key<'a, T, U>(
    graph: &'a HashMap<T, U>,
    get_key: &impl Fn(&U) -> &T,
    worklist: &mut Vec<&'a T>,
    visited: &mut HashMap<&'a T, Status>,
) -> Option<Cycle<'a, U>>
where
    T: Eq + Hash,
    U: Eq + Hash,
{
    // @Task rewrite to avoid ugly unwrap
    let entry = graph.get(worklist.last().unwrap())?;
    let node = get_key(entry);

    let cycle = match visited.get(&node) {
        Some(Status::Ongoing) => {
            let cycle: Cycle<'_, U> = worklist
                .iter()
                .copied()
                .skip_while(|&some_node| some_node != node)
                .map(|some_node| &graph[some_node])
                .collect();

            (!cycle.is_empty()).then(|| cycle)
        }
        Some(Status::Finished) => None,
        None => {
            worklist.push(node);
            visited.insert(node, Status::Ongoing);
            find_cycle_by_key(graph, get_key, worklist, visited)
        }
    };

    // @Task rewrite to avoid ugly unwrap
    visited.insert(worklist.pop().unwrap(), Status::Finished);
    cycle
}

pub type Graph<T> = HashMap<T, T>;
pub type Cycle<'a, T> = HashSet<&'a T>;

pub fn find_cycles<T>(graph: &Graph<T>) -> Vec<Cycle<'_, T>>
where
    T: Eq + Hash,
{
    let mut cycles = Vec::new();
    let mut visited = HashMap::default();

    for node in graph.keys() {
        if let Entry::Vacant(entry) = visited.entry(node) {
            entry.insert(Status::Ongoing);
            cycles.extend(find_cycle(graph, &mut vec![node], &mut visited));
        }
    }

    cycles
    // find_cycles_by_key(graph, |node| node)
}

fn find_cycle<'a, T>(
    graph: &'a Graph<T>,
    worklist: &mut Vec<&'a T>,
    visited: &mut HashMap<&'a T, Status>,
) -> Option<Cycle<'a, T>>
where
    T: Eq + Hash,
{
    // @Task rewrite to avoid ugly unwrap
    let node = graph.get(worklist.last().unwrap())?;

    let cycle = match visited.get(&node) {
        Some(Status::Ongoing) => {
            let cycle: Cycle<'_, _> = worklist
                .iter()
                .copied()
                .skip_while(|&some_node| some_node != node)
                .collect();

            (!cycle.is_empty()).then(|| cycle)
        }
        Some(Status::Finished) => None,
        None => {
            worklist.push(node);
            visited.insert(node, Status::Ongoing);
            find_cycle(graph, worklist, visited)
        }
    };

    // @Task rewrite to avoid ugly unwrap
    visited.insert(worklist.pop().unwrap(), Status::Finished);
    cycle
}

enum Status {
    Ongoing,
    Finished,
}

#[cfg(test)]
mod tests {
    use super::{find_cycles, Cycle, Graph};
    use crate::HashMap;

    #[test]
    fn empty_graph() {
        assert_eq!(find_cycles(&Graph::<()>::default()), Vec::new());
    }

    #[test]
    fn no_cycle_0() {
        assert_eq!(
            find_cycles(&HashMap::from_iter([(1, 4), (4, 9), (3, 8)])),
            Vec::new(),
        );
    }

    #[test]
    fn no_cycle_1() {
        assert_eq!(
            find_cycles(&HashMap::from_iter([(8, 0), (20, 0), (1, 0)])),
            Vec::new(),
        );
    }

    #[test]
    fn cycle_size_one() {
        assert_eq!(
            find_cycles(&HashMap::from_iter([(0, 0)])),
            vec![Cycle::from_iter([&0])],
        );
    }

    #[test]
    fn cycle_size_two() {
        assert_eq!(
            find_cycles(&HashMap::from_iter([(5, 90), (90, 5)])),
            vec![Cycle::from_iter([&5, &90])],
        );
    }

    #[test]
    fn large_cycle() {
        assert_eq!(
            find_cycles(&HashMap::from_iter([
                (6, 5),
                (5, 4),
                (22, 5),
                (4, 3),
                (3, 2),
                (2, 1),
                (108, 109),
                (1, 6),
            ])),
            vec![Cycle::from_iter([&1, &2, &3, &4, &5, &6])],
        );
    }

    #[test]
    fn two_cycles() {
        assert_eq!(
            find_cycles(&HashMap::from_iter([
                (1, 2),
                (10, 9),
                (2, 3),
                (3, 1),
                (9, 10)
            ])),
            vec![Cycle::from_iter([&9, &10]), Cycle::from_iter([&1, &2, &3])],
        );
    }
}
