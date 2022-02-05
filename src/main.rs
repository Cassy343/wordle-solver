use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::Mutex;
use std::{
    fmt::{self, Debug, Formatter},
    fs, str,
};
use std::io::stdin;

const WORD_SIZE: usize = 5;

fn to_word(word: &str) -> Word<WORD_SIZE> {
    Word::new(word.as_bytes().to_owned().try_into().unwrap())
}

fn main() {
    let dict = fs::read_to_string("dictionary.txt").unwrap();
    let mut dict = dict
        .split('\n')
        .filter(|&word| word.len() == WORD_SIZE)
        .map(to_word)
        .map(|word| (word, 1f64))
        .collect::<HashMap<Word<WORD_SIZE>, f64>>();
    let wordle_dict = fs::read_to_string("wordle-dictionary.txt").unwrap();
    let mut candidate_words = Vec::new();
    wordle_dict
        .split('\n')
        .filter(|&word| word.len() == WORD_SIZE)
        .map(to_word)
        .for_each(|word| {
            candidate_words.push(word);
            dict.insert(word, 1f64);
        });

    let mut guess_words = dict.keys().copied().collect::<Vec<_>>();

    println!("Guess \"roate\"");
    let mut word = Word::<WORD_SIZE>::from_str("roate");

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
                word = guess_words[index];
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

        candidate_words = candidate_words.into_iter().filter(|word| word.test(&mask)).collect();
        println!("Considering {} possible words", candidate_words.len());

        if candidate_words.len() == 1 {
            println!("Guess {:?}", candidate_words[0]);
            return;
        }

        // if guess_words.len() <= 3 {
        //     guess_words = candidate_words.clone();
        // }

        let min: Mutex<usize> = Mutex::new(usize::MAX);

        let map = guess_words
            .par_iter()
            .map(|guess| {
                let mut total = 0;
                let bound = *min.lock().unwrap();

                for actual in &candidate_words {
                    let mask = guess.diff(actual);

                    let portion = candidate_words.iter().filter(|sample| sample.test(&mask)).count();
                    total += portion;

                    if total > bound {
                        return (*guess, usize::MAX);
                    }
                }

                let mut bound = min.lock().unwrap();
                if total < *bound {
                    *bound = total;
                }
                drop(bound);

                (*guess, total)
            })
            .collect::<HashMap<Word<WORD_SIZE>, usize>>();
        guess_words.sort_unstable_by(|a, b| {
            let a_score = map.get(a).copied().unwrap();
            let b_score = map.get(b).copied().unwrap();
            a_score
                .cmp(&b_score)
        });

        first = false;
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
                    assert!(offset[ch as usize] < 4);
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
                assert!(inv_counts[ch_index] < 4);
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
                'x' => Constraint::Fixed(word.chars[index]),
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
