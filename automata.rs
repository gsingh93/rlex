#![feature(macro_rules)]

extern crate collections;

use std::fmt;
use std::fmt::Show;
use std::hash::Hash;
use collections::HashMap;
use collections::HashSet;

struct Automaton<S, I> {
    alphabet: Vec<I>,
    initial_state: S,
    final_states: HashSet<S>,
    trans_table: HashMap<S, HashMap<I, S>>
}

trait Concat<T> {
    fn concat(&mut self, a: &T);
}

impl Concat<String> for String {
    fn concat(&mut self, a: &String) {
        self.push_str(a.as_slice());
    }
}

macro_rules! set(
    ($($elem:expr),*) => ({
        let mut s = collections::HashSet::new();
        $( s.insert($elem); ),*
        s
    })
)

trait HashElt: Hash + Eq + TotalEq {}
impl<T: Hash + Eq + TotalEq> HashElt for T {}

impl<S: HashElt + Clone + Show, I: HashElt + Clone + Show> Automaton<S, I> {
    fn new(initial_state: S, alphabet: Vec<I>) -> Automaton<S, I> {
        let h: HashMap<S, HashMap<I, S>> = HashMap::new();
        Automaton {alphabet: alphabet,
                   initial_state: initial_state.clone(),
                   final_states: HashSet::new(),
                   trans_table: h}
    }

    fn insert_transition(&mut self, from: &S, to: &S,
                         input: &I) -> Result<(), String> {
        let h = self.trans_table.find_or_insert(from.clone(), HashMap::new());
        if h.contains_key(input) {
            return Err("Multiple transitions for a single input not \
                       allowed in a DFA".to_string());
        }
        h.insert((*input).clone(), to.clone());
        Ok(())
    }

    fn move(&self, state: &S, input: &I) -> Result<S, String> {
        if !self.alphabet.contains(input) {
            return Err("Input not part of alphabet".to_string());
        }
        if !self.trans_table.contains_key(state) {
            return Err(format!("No valid transition from state {} given input {}",
                    state, input));
        }
        let table = self.trans_table.get(state);
        if !table.contains_key(input) {
            return Err(format!("No valid transition from state {} given input {}",
                               state, input));
        }
        Ok(table.get_copy(input))
    }

    fn eclosure(&self, state: &S) -> HashSet<S> {
        set!(state.clone())
    }

    fn simulate(&self, inputs: Vec<I>) -> Result<(), String> {
        let mut cur_state = self.initial_state.clone();
        for input in inputs.iter() {
            println!("Current state is {}", cur_state);
            cur_state = try!(self.move(&cur_state, input));
            println!("Moved to state {} due to input {}", cur_state, input);
        }
        println!("Final state is {}", cur_state);
        if self.final_states.contains(&cur_state) {
            println!("{}", "Input accepted");
        } else {
            println!("{}", "Input not accepted");
        }
        Ok(())
    }
}

impl<S: Show + HashElt, I> Show for Automaton<S, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Initial State: {}\n", self.initial_state));
        try!(write!(f, "Final States: "));
        for s in self.final_states.iter() {
            try!(write!(f, "{} ", s));
        }
        Ok(())
    }
}

fn to_state_name<S: HashElt + Clone + Concat<S>>(set: &HashSet<S>) -> S {
    assert!(set.len() > 0);
    let mut result = (*set.iter().nth(1).unwrap()).clone();
    for s in set.iter() {
        result.concat(s);
    }
    result
}

fn nfa_to_dfa<S: Clone + HashElt + Show + Concat<S>, I: HashElt + Clone + Show> (nfa: &Automaton<S, I>) -> Result<(), String> {
    let mut dfa = Automaton::new(nfa.initial_state.clone(), Vec::new());

    // The states in the resulting DFA
    let eclosure: HashSet<S> = nfa.eclosure(&nfa.initial_state);
    let mut states: HashSet<S> = set!(to_state_name(&eclosure));

    // All states in the resulting DFA we haven't visited
    let mut unvisited: Vec<HashSet<S>> = vec!(set!(nfa.initial_state.clone()));

    while !unvisited.is_empty() {
        // The subset of states in the NFA that make up a single state in the DFA
        let state = unvisited.pop().unwrap();

        // See where we can end up for each possible input
        for input in nfa.alphabet.iter() {
            // Calculate the epsilon closure of each NFA state in our subset
            let mut closure: HashSet<S> = HashSet::new();
            for node in state.iter() {
                // If the input moves us to a new state, add it to the closure
                // Otherwise, try the next state
                let to = match nfa.move(node, input) {
                    Ok(to) => to,
                    Err(_) => continue // TODO
                };
                // Union the current set of states with the new states
                closure = closure.union(&nfa.eclosure(&to))
                    .map(|x| (*x).clone()).collect();
            }

            // If our resulting state is not a DFA state, add it
            let new_state = to_state_name(&closure);
            if !states.contains(&new_state) {
                states.insert(new_state.clone());
            }

            // Add a transition from the current state to the new state
            let cur_state = to_state_name(&state);
            try!(dfa.insert_transition(&cur_state, &new_state, &input))
        }
    }
    Ok(())
}

#[test]
fn automaton_insert_non_existant_state_test() {
    let mut g = Automaton::new("A", vec!("A"));
    assert!(g.insert_transition(&"A", &"B", &"A").is_err());

    let mut g = Automaton::new("A", vec!("A"));
    assert!(g.insert_transition(&"B", &"A", &"A").is_err());
}

#[test]
fn automaton_insert_not_in_alphabet_test() {
    let mut g = Automaton::new("A", vec!("A"));
    assert!(g.insert_transition(&"A", &"A", &"B").is_err());
}

#[test]
fn automaton_simulate_not_in_alphabet_test() {
    let g = Automaton::new("A", vec!("A"));
    assert!(g.simulate(vec!("B")).is_err());
}

#[test]
fn automaton_simulate_no_transitions_test() {
    let g = Automaton::new("A", vec!("A"));
    assert!(g.simulate(vec!("A")).is_err());
}

#[test]
fn automaton_simulate_accept_test() {
    let mut g = Automaton::new("A", vec!("A"));
    assert!(g.insert_transition(&"A", &"B", &"A").is_ok());
    g.final_states.insert("B");
    g.simulate(vec!("A"));

    let mut g = Automaton::new("A", vec!("A"));
    g.final_states.insert("A");
    g.simulate(vec!(""));
}

#[test]
fn automaton_simulate_reject_test() {
    let mut g = Automaton::new("A", vec!("A"));
    assert!(g.insert_transition(&"A", &"B", &"A").is_ok());
    g.final_states.insert("B");
    g.simulate(vec!(""));

    let mut g = Automaton::new("A", vec!("A"));
    assert!(g.insert_transition(&"A", &"B", &"A").is_ok());
    assert!(g.insert_transition(&"B", &"C", &"A").is_ok());
    g.final_states.insert("B");
    g.simulate(vec!("AA"));
}

#[test]
fn nfa_to_dfa_empty_test() {
    let g = Automaton::new("B".to_string(), vec!("A"));
    assert!(nfa_to_dfa(&g).is_ok());
    // TODO: Assert empty
}

#[test]
fn nfa_to_dfa_already_dfa_test() {
    let mut g = Automaton::new("B".to_string(), vec!("A"));
    assert!(g.insert_transition(&"B".to_string(), &"C".to_string(), &"A")
            .is_ok());
    g.final_states.insert("C".to_string());
    assert!(nfa_to_dfa(&g).is_ok());
    // TODO: Assert same
}

#[test]
fn nfa_to_dfa_test() {
    // TODO
}
