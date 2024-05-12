use std::sync::{Arc, Mutex};
use std::thread;

const NUM_PHILOSOPHERS: usize = 12;
const MEALS_TO_EAT: usize = 3;

struct Philosopher {
    id: usize,
    left_chopstick: Arc<Mutex<()>>,
    right_chopstick: Arc<Mutex<()>>,
    meals_eaten: usize,
}

impl Philosopher {
    fn new(id: usize, left_chopstick: Arc<Mutex<()>>, right_chopstick: Arc<Mutex<()>>) -> Philosopher {
        Philosopher {
            id,
            left_chopstick,
            right_chopstick,
            meals_eaten: 0,
        }
    }

    fn eat(&mut self) {
        while self.meals_eaten < MEALS_TO_EAT {
            // Lock the left and right chopsticks for the philosopher to use
            let _left = self.left_chopstick.lock().unwrap();
            let _right = self.right_chopstick.lock().unwrap();

            // Print a message indicating the philosopher is eating
            println!("Philosopher {} is eating meal {}", self.id, self.meals_eaten + 1);
            // Increment the number of meals eaten
            self.meals_eaten += 1;

            // Simulate the time it takes for the philosopher to eat
            thread::sleep(std::time::Duration::from_millis(100));

            // Print a message indicating the philosopher has finished eating
            println!("Philosopher {} finished meal {}", self.id, self.meals_eaten);

            // Release the chopsticks so they can be used by others
            drop(_left);
            drop(_right);
        }
    }
}

fn main() {
    // Create a vector of chopsticks for the philosophers to use
    let chopsticks: Vec<_> = (0..NUM_PHILOSOPHERS).map(|_| Arc::new(Mutex::new(()))).collect();
    let mut handles = vec![];

    // Create a vector of philosophers, each with a left and right chopstick
    let philosophers: Vec<_> = (0..NUM_PHILOSOPHERS).map(|i| {
        let left = chopsticks[i].clone();
        let right = chopsticks[(i + 1) % NUM_PHILOSOPHERS].clone();
        Philosopher::new(i, left, right)
    }).collect();

    // For each philosopher, spawn a new thread where they start eating
    for mut philosopher in philosophers {
        let handle = thread::spawn(move || {
            philosopher.eat();
        });
        // Store the handle for the thread
        handles.push(handle);
    }

    // Wait for all threads to finish
    for handle in handles {
        handle.join().unwrap();
    }

    // Print a message indicating all philosophers have finished eating
    println!("All philosophers finished eating");
}
