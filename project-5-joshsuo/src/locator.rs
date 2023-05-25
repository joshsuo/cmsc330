use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/*
    An optional definition of a Node struct you may find useful
*/
struct Node<T> {
    priority: i32,
    data: T,
}

/* 
    These traits are implemented for Nodes to make them comparable 
*/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}


/*
    You must implement the above trait for the vector type 
*/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /*
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    */
    fn enqueue(&mut self, ele: T) -> () {
        self.push(ele);
        let mut curr = self.len()-1;

        while curr != 0 {
            if self[(curr-1)/2] > self[curr] {
                self.swap((curr-1)/2, curr);
                curr = (curr-1)/2;
            }
            else {
                curr = 0;
            }
        }
    }

    /*
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    */
    /*
    fn dequeue(&mut self) -> Option<T> {
        if self.len() == 0 {
            return None
        }

        let mut len = self.len()-1;
        self.swap(0, len);
        let min = self.remove(len);
        let full = self.len();
        len = full -1;
        let mut curr = 0;

        while curr < full {
            let left = 2*curr + 1;
            let right = 2*curr + 2;
            if left < len && right < len {
                if self[left] <= self[right] {
                    if self[curr] > self[left] {
                        self.swap(curr, left);
                        curr = left;
                    }
                    else {
                        curr = full;
                    }
                }
                else {
                    if self[curr] > self[right] {
                        self.swap(curr, right);
                        curr = right;
                    }
                    else {
                        curr = full;
                    }
                }
            } else if left < len && right >= len {
                if self[curr] > self[left] {
                    self.swap(curr, left);
                    curr = left;
                }
                else {
                    curr = full;
                }
            } else if right < len {
                if self[curr] > self[right] {
                    self.swap(curr, right);
                    curr = right;
                }
                else {
                    curr = full;
                }
            } else if left == len {
              if self[curr] > self[left] {
                  self.swap(curr, left);
              }  
              curr = full;
            } else {
                curr = full;
            }
        }
        return Some (min)
    }
    */
    
    
    fn dequeue(&mut self) -> Option<T> {
        if self.len() == 0 {
            return None
        }

        let len = self.len()-1;
        self.swap(0, len);
        let min = self.remove(len);
        let full = self.len();
        if full == 0 {
            return Some (min)
        }
        let last_index = self.len() - 1;
        let mut curr = 0;

        while curr < full {
            let left = 2*curr + 1;
            let right = 2*curr + 2;
            if right <= last_index {
                if self[left] <= self[right] {
                    if self[curr] > self[left] {
                        self.swap(curr, left);
                        curr = left;
                    }
                    else {
                        curr = full;
                    }
                }
                else {
                    if self[curr] > self[right] {
                        self.swap(curr, right);
                        curr = right;
                    }
                    else {
                        curr = full;
                    }
                }
            } else if left == last_index {
                if self[curr] > self[left] {
                    self.swap(curr, left);
                }
                curr = full;
            } else {
                curr = full;
            }
        }
        return Some (min)
    }
    

    /*
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    */
    fn peek(&self) -> Option<&T> {
        if self.len() == 0 {
            return None
        }
        return Some (&self[0])
    }
}


/*
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
*/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    let res = (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs();
    return res;
}

/*
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
*/
pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {
    let mut pairs = Vec::new();
    let mut fight_pairs = Vec::new();

    for (ally, a_pos) in allies {
        for (enemy, e_pos) in enemies {
            let pair = Node {
                priority: distance(*a_pos, *e_pos),
                data: (ally, enemy, e_pos)
            };
            pairs.enqueue(pair)
        }
    }

    while pairs.len() != 0 {
        let closest = pairs.dequeue();
        match closest {
            None => return ("no fight", 0, 0),
            Some (dist) => {
                if dist.data.0 == &"Stark" && fight_pairs.contains(dist.data.1) == false {
                    return (dist.data.1, dist.data.2.0, dist.data.2.1)
                }
                else if fight_pairs.contains(dist.data.0) == false && fight_pairs.contains(dist.data.1) == false {
                    fight_pairs.push(dist.data.0);
                    fight_pairs.push(dist.data.1);
                }
            }
        }
    }
    return ("no fight", 0, 0)
}


