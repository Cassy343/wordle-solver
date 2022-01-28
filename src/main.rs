use once_cell::sync::OnceCell;
use rayon::prelude::*;
use std::collections::HashMap;
use std::str::FromStr;
use std::sync::atomic::AtomicU64;
use std::{
    fmt::{self, Debug, Formatter},
    fs,
    io::stdin,
    str,
};
use std::sync::atomic::Ordering;

const WORD_SIZE: usize = 5;
static DICT: OnceCell<Dictionary<WORD_SIZE>> = OnceCell::new();

fn d() -> &'static Dictionary<WORD_SIZE> {
    unsafe { DICT.get_unchecked() }
}

fn main() {
    let dict = fs::read_to_string("dictionary.txt").unwrap();
    let mut dict = dict
        .split('\n')
        .filter(|&word| word.len() == WORD_SIZE)
        .map(|word| {
            word.as_bytes()
                .to_owned()
                .try_into()
                .unwrap()
        })
        .map(Word::new)
        .map(|word| (word, 1f64))
        .collect::<HashMap<Word<WORD_SIZE>, f64>>();
    let mut words = dict.keys().copied().collect::<Vec<_>>();

    let freq = fs::read_to_string("freq.txt").unwrap();

    for line in freq.split('\n') {
        let index = match line
            .char_indices()
            .find(|&(_, ch)| ch == ' ')
            .map(|(index, _)| index)
        {
            Some(index) => index,
            None => continue,
        };

        let f = match f64::from_str(line[..index].trim()) {
            Ok(f) => f,
            Err(_) => continue,
        };
        let w = line[index + 1..].trim();

        if w.len() != WORD_SIZE {
            continue;
        }

        let w = Word::<WORD_SIZE>::from_str(w);
        if let Some(wf) = dict.get_mut(&w) {
            *wf = f;
        }
    }

    let total = dict.values().copied().sum::<f64>();
    dict.values_mut().for_each(|f| *f /= total);

    DICT.set(Dictionary { dict }).ok().unwrap();

    println!("Guess \"irate\"");
    let mut word = Word::<WORD_SIZE>::from_str("irate");

    macro_rules! wrong_input {
        () => {{
            println!("Please enter a string of five characters. + = correct, o = contains, . = incorrect");
            continue;
        }}
    }

    macro_rules! read_line {
        ($input:expr) => {{
            $input.clear();
            stdin().read_line(&mut $input).expect("Invalid input");
            let new_len = $input.len() - 1;
            $input.truncate(new_len);
        }};
    }

    let mut input = String::new();
    let mut first = true;

    loop {
        if !first {
            let mut index = 0;
            loop {
                word = words[index];
                println!("Guess \"{:?}\"", word);
                read_line!(input);

                if input.starts_with('=') {
                    match Word::<WORD_SIZE>::from_str_checked(&input[1 ..]) {
                        Some(replacement) => {
                            word = replacement;
                            read_line!(input);
                            break;
                        },
                        None => {
                            println!("Please enter a {}-letter word", WORD_SIZE);
                            continue;
                        }
                    }
                } else if input != "pass" {
                    break;
                } else {
                    index += 1;
                }
            }
        } else {
            read_line!(input)
        }

        if input == "done" || input.trim().is_empty() {
            return;
        }

        if input.len() != WORD_SIZE {
            wrong_input!()
        }

        let mask = match Mask::new(&word, &input) {
            Some(mask) => mask,
            None => {
                wrong_input!()
            }
        };

        words = words.into_iter().filter(|word| word.test(&mask)).collect();
        println!("Considering {} possible words", words.len());

        if words.len() <= 10 {
            words.sort_unstable_by(|a, b| {
                let dict = d();
                dict.prob(b).partial_cmp(&dict.prob(a)).unwrap()
            });
            continue;
        }

        let min: AtomicU64 = AtomicU64::new(f64::MAX.to_bits());

        let map = words
            .par_iter()
            .map(|guess| {
                let mut total = 0.0f64;
                let bound = f64::from_bits(min.load(Ordering::Acquire));

                for actual in &words {
                    let mask = guess.diff(actual);

                    let portion = words.iter().filter(|sample| sample.test(&mask)).count();
                    let actual_prob = d().prob(actual);
                    total += actual_prob * (portion as f64 / words.len() as f64);

                    if total > bound {
                        return (*guess, f64::MAX);
                    }
                }

                let raw_bound = min.load(Ordering::Acquire);
                let bound = f64::from_bits(raw_bound);
                let new_bound = total.to_bits();
                if total < bound {
                    let _ = min.compare_exchange_weak(
                        raw_bound,
                        new_bound,
                        Ordering::Release,
                        Ordering::Relaxed
                    );
                }

                (*guess, total)
            })
            .collect::<HashMap<Word<WORD_SIZE>, f64>>();
        words.sort_unstable_by(|a, b| {
            let a_score = map.get(a).copied().unwrap_or(1.0f64);
            let b_score = map.get(b).copied().unwrap_or(1.0f64);
            let ordering = a_score
                .partial_cmp(&b_score)
                .unwrap_or(std::cmp::Ordering::Equal);
            if ordering == std::cmp::Ordering::Equal {
                let dict = d();
                dict.prob(b).partial_cmp(&dict.prob(a)).unwrap()
            } else {
                ordering
            }
        });

        first = false;
    }
}

struct Dictionary<const N: usize> {
    dict: HashMap<Word<N>, f64>,
}

impl<const N: usize> Dictionary<N> {
    fn prob(&self, word: &Word<N>) -> f64 {
        self.dict.get(word).copied().unwrap_or(0.0)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct Word<const N: usize> {
    chars: [u8; N],
}

impl<const N: usize> Word<N> {
    pub fn new(mut chars: [u8; N]) -> Self {
        for ch in chars.iter_mut() {
            *ch -= 'a' as u8;
        }

        Self { chars }
    }

    pub fn from_str_checked(string: &str) -> Option<Self> {
        if string.len() != N {
            return None;
        }

        let mut chars = [0u8; N];
        for (index, ch) in string.chars().enumerate() {
            chars[index] = ch as u8 - 'a' as u8;
        }

        Some(Self { chars })
    }

    pub fn from_str(string: &str) -> Self {
        Self::from_str_checked(string).unwrap()
    }

    pub fn contents(&self, mask: &Mask<N>) -> u128 {
        let mut offset = [0u8; 26];

        self.chars
            .iter()
            .copied()
            .enumerate()
            .map(|(index, ch)| {
                if matches!(mask.constraints[index], Constraint::Fixed(_)) {
                    0
                } else {
                    let value = 1u128 << (4 * ch + offset[ch as usize]);
                    offset[ch as usize] += 1;
                    value
                }
            })
            .fold(0, |a, b| a | b)
    }

    pub fn diff(&self, actual: &Self) -> Mask<N> {
        let mut constraints = [Constraint::NotContains(0, 0); N];
        let mut counts = [0u8; 26];
        let mut inv_counts = [0u8; 26];

        for (index, ch) in actual.chars.iter().copied().enumerate() {
            if self.chars[index] == ch {
                constraints[index] = Constraint::Fixed(ch)
            } else {
                counts[ch as usize] += 1;
            }
        }

        for (index, ch) in self.chars.iter().copied().enumerate() {
            let ch_index = ch as usize;

            let constraint = if matches!(constraints[index], Constraint::Fixed(_)) {
                constraints[index]
            } else if counts[ch_index] > 0 {
                counts[ch_index] -= 1;
                let c = Constraint::Contains(ch, inv_counts[ch_index]);
                inv_counts[ch_index] += 1;
                c
            } else {
                Constraint::NotContains(ch, inv_counts[ch_index])
            };

            constraints[index] = constraint;
        }

        Mask { constraints }
    }

    fn test(&self, mask: &Mask<N>) -> bool {
        let contents = self.contents(mask);

        for (index, constraint) in mask.iter().enumerate() {
            match constraint {
                Constraint::NotContains(ch, offset) => {
                    if (contents & (1 << (4 * ch + offset))) != 0 {
                        return false;
                    }
                }
                Constraint::Fixed(ch) => {
                    if ch != self.chars[index] {
                        return false;
                    }
                }
                Constraint::Contains(ch, offset) => {
                    if ch == self.chars[index] || (contents & (1 << (4 * ch + offset))) == 0 {
                        return false;
                    }
                }
            }
        }

        true
    }
}

impl<const N: usize> Debug for Word<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut chars = self.chars;
        for ch in chars.iter_mut() {
            *ch += 'a' as u8;
        }
        write!(f, "{}", unsafe {
            str::from_utf8_unchecked(chars.as_slice())
        })
    }
}

#[derive(Debug)]
struct Mask<const N: usize> {
    constraints: [Constraint; N],
}

impl<const N: usize> Mask<N> {
    fn new(word: &Word<N>, pat: &str) -> Option<Self> {
        let mut constraints = [Constraint::Fixed(0); N];
        let mut counts = [0u8; 26];

        for (index, ch) in pat.chars().enumerate() {
            constraints[index] = match ch {
                '+' => Constraint::Fixed(word.chars[index]),
                'o' => {
                    let c =
                        Constraint::Contains(word.chars[index], counts[word.chars[index] as usize]);
                    counts[word.chars[index] as usize] += 1;
                    c
                }
                '.' => {
                    Constraint::NotContains(word.chars[index], counts[word.chars[index] as usize])
                }
                _ => return None,
            }
        }

        Some(Self { constraints })
    }

    fn iter(&self) -> impl ExactSizeIterator<Item = Constraint> + '_ {
        self.constraints.iter().copied()
    }
}

#[derive(Clone, Copy, Debug)]
enum Constraint {
    Fixed(u8),
    Contains(u8, u8),
    NotContains(u8, u8),
}
