/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    let mut result = 0;

    if n < 0 {
        result = -1;
    }
    else {
        for i in 1..=n {
            result += i;
        }
    }
    return result
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut num = 0;

    if s > e {
        return 0
    }
    for i in ls {
        if i >= &s && i <= &e {
            num += 1;
        }
    }
    return num
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    if target == [] {
        return true
    }
    for i in target {
        if set.contains(i) == false {
            return false
        }
    }
    return true
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls == [] {
        return None
    }
    let mut total = 0 as f64;
    for i in ls {
        total += i;
    }
    return Some(total/(ls.len() as f64))
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    ls.iter().fold(0, |acc, &elem| acc*2 + elem)
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut num = n;
    let mut div = 2;
    let mut arr = Vec::new();

    while is_prime(num) == false {
        if num % div == 0 {
            num = num/div;
            arr.push(div);
        }
        else {
            div += 1;
        }
    }
    arr.push(num);
    return arr
}

//helper function
fn is_prime(n: u32) -> bool {
    if n <= 1 {
        return false;
    }
    for a in 2..n {
        if n % a == 0 {
            return false; 
        }
    }
    return true
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut new_vec = Vec::new();

    if lst.len() == 0 {
        return new_vec
    }

    let first = lst[0];
    for i in 1..lst.len() {
        new_vec.push(lst[i])
    }
    new_vec.push(first);
    return new_vec
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    if target == ""{
        return true
    }
    if s.len() < target.len() {
        return false
    }

    for i in 0..s.len() {
        if (s.len() - i) < target.len() {
            return false
        }
        if &s[i..(i+target.len())] == target {
            return true
        }
    }
    return false
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
// pub fn longest_sequence(s: &str) -> Option<&str> {
//     let mut long = "";
//     let mut temp = "";

//     if s == "" {
//         return None
//     }

//     for i in 0..s.len() {
//         if curr == &s[i] {
//             count += 1;
//         }
//     }
// }


pub fn longest_sequence(s: &str) -> Option<&str> {
    let mut index = 0;
    let mut end_index = 0;
    let mut temp_index = 0;
    let mut temp_end_index = 0;
    let mut count = 0;
    let mut temp_count = 0;
    let mut curr = "";

    if s == "" {
        return None;
    }

    for i in 0..s.len() {
        if curr == &s[i..=i] {
            temp_count += 1;
            temp_end_index += 1;
        }
        else { 
            curr = &s[i..=i];
            temp_index = i;
            temp_end_index = i;
            temp_count = 1;
        }
        
        if temp_count > count {
            count = temp_count;
            end_index = temp_end_index;
            index = temp_index;
        }
    }

    return Some (&s[index..=end_index]);
}
