#![feature(macro_rules)]

use std::fmt::{self, Display};
use std::hash::Hash;
use std::collections::{HashSet, HashMap};

macro_rules! set(
    ($($elem:expr),*) => ({
        let mut s = HashSet::new();
        $( s.insert($elem); ),*
        s
    })
);


trait HashElt: Hash + Eq {}
impl<T: Hash + Eq> HashElt for T {}

trait Concat<T> {
    fn concat(&mut self, a: &T);
}

impl Concat<String> for String {
    fn concat(&mut self, a: &String) {
        self.push_str(a.as_slice());
    }
}

struct Automaton<S, I> {
    alphabet: Vec<I>,
    initial_state: S,
    final_states: HashSet<S>,
    trans_table: HashMap<S, HashMap<I, S>>
}

struct Simulator<'a, S: 'a, I: 'a, It: Iterator<Item = &'a I>> {
    automaton: &'a Automaton<S, I>,
    inputs: It,
    cur_state: S
}

impl<S: HashElt + Clone + Display, I: HashElt + Clone + Display> Automaton<S, I> {
    fn new(initial_state: S, alphabet: Vec<I>) -> Automaton<S, I> {
        let h: HashMap<S, HashMap<I, S>> = HashMap::new();
        Automaton {alphabet: alphabet,
                   initial_state: initial_state.clone(),
                   final_states: HashSet::new(),
                   trans_table: h}
    }

    fn insert_transition(&mut self, from: &S, to: &S,
                         input: &I) -> Result<(), String> {
        let h = match self.trans_table.entry(from.clone()).get() {
            Ok(v) => *v,
            Err(e) => *e.insert(HashMap::new())
        };
        //let h = self.trans_table.find_or_insert(from.clone(), HashMap::new());
        if h.contains_key(input) {
            return Err("Multiple transitions for a single input not \
                       allowed in a DFA".to_string());
        }
        h.insert((*input).clone(), to.clone());
        Ok(())
    }

    fn consume(&self, state: &S, input: &I) -> Result<S, String> {
        if !self.alphabet.contains(input) {
            return Err("Input not part of alphabet".to_string());
        }
        let table = match self.trans_table.get(state) {
            Some(t) => t,
            None => return Err(format!("No valid transition from state {} given input {}",
                                       state, input))
        };
        match table.get(input) {
            Some(s) => Ok(s.clone()),
            None => Err(format!("No valid transition from state {} given input {}",
                                state, input))
        }
    }

    fn eclosure(&self, state: &S) -> HashSet<S> {
        set!(state.clone())
    }

    fn simulate<'a, It: Iterator<Item = &'a I>>(&self, inputs: Vec<I>) -> Result<(), String> {
        let mut sim: Simulator<S, I, It> = Simulator::new(self, inputs.iter());

        let mut cur_state = &sim.cur_state.clone();
        println!("Current state is {}", cur_state);

        loop {
            let res = sim.step();
            if res.is_none() {
                break;
            }

            let (cur_state, input) = try!(res.unwrap());
            println!("Moved to state {} due to input {}", cur_state, input);
        }

        println!("Final state is {}", cur_state);
        if self.final_states.contains(cur_state) {
            println!("{}", "Input accepted");
        } else {
            println!("{}", "Input not accepted");
        }
        Ok(())
    }
}

impl<S: Display + HashElt, I> Display for Automaton<S, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Initial State: {}\n", self.initial_state));
        try!(write!(f, "Final States: "));
        for s in self.final_states.iter() {
            try!(write!(f, "{} ", s));
        }
        Ok(())
    }
}

impl<'a, S: HashElt + Clone + Display, I: HashElt + Clone + Display, It: Iterator<Item = &'a I>> Simulator<'a, S, I, It> {
    fn new(automaton: &'a Automaton<S, I>, inputs_iter: It) -> Simulator<'a, S, I, It> {
        let initial_state = automaton.initial_state.clone();
        Simulator { automaton: automaton, inputs: inputs_iter,
                    cur_state: initial_state }
    }

    fn step(&mut self) -> Option<Result<(S, I), String>> {
        let input = self.inputs.next();
        if input.is_none() {
            return None;
        }
        self.cur_state = match self.automaton.consume(&self.cur_state, input.unwrap()) {
            Ok(state) => state,
            Err(e) => return Some(Err(e))
        };
        Some(Ok((self.cur_state.clone(), (*input.unwrap()).clone())))
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

fn nfa_to_dfa<S: Clone + HashElt + Display + Concat<S>, I: HashElt + Clone + Display> (nfa: &Automaton<S, I>) -> Result<(), String> {
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
                let to = match nfa.consume(node, input) {
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

#[cfg(test)]
mod test {
    type State = String;

    struct FSM {
        initial_state: State
    }

    impl FSM {
        fn consume(&self, cur_state: &State, input: &String) -> Option<State> {
            match (cur_state.as_slice(), input.as_slice()) {
                ("A", "A") => Some("B".to_string()),
                ("B", "B") => Some("C".to_string()),
                ("C", _)   => None, // C is the final state
                _          => None
            }
        }
    }

    struct Simulator<'a> {
        cur_state: Option<State>,
        fsm: &'a FSM,
        inputs: Box<Iterator<&'a String>>
    }

    impl<'a> Simulator<'a> {
        fn step(&mut self) -> Option<(State, String)> {
            // Initial call should return initial state and consume no input
            if self.cur_state.is_none() {
                self.cur_state = Some(self.fsm.initial_state.clone());
                return Some((self.fsm.initial_state.clone(), "".to_string()));
            }
            let input = self.inputs.next();
            if input.is_none() {
                return None; // Finished iterating
            }

            // We know cur_state and input are not None at this point so we
            // can unwrap them
            self.cur_state = self.fsm.move(self.cur_state.get_ref(),
                                           input.unwrap());
            if self.cur_state.is_none() {
                return None; // No transitions from this state
            }
            Some((self.cur_state.get_ref().clone(), input.unwrap().to_string()))
        }
    }

    #[test]
    fn test_main() {
        let fsm = FSM { initial_state: "A".to_string() };
        let v = vec!("A".to_string(),
                     "B".to_string());
        let mut sim = Simulator { cur_state: None, fsm: &fsm,
                                  inputs: box v.iter() };
        loop {
            match sim.step() {
                Some((new_state, _)) => println!("Current state is {}", new_state),
                None => break
            };
        }
    }

}
