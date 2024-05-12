package main

import (
	"fmt"
	"sync"
	"time"
)

const (
	numPhilosophers = 12
	mealsToEat      = 3
)

type Chopstick struct{ sync.Mutex }

type Philosopher struct {
	id                            int
	leftChopstick, rightChopstick *Chopstick
	mealsEaten                    int
}

func (p *Philosopher) eat(wg *sync.WaitGroup, sem *Semaphore) {
	defer wg.Done()
	for p.mealsEaten < mealsToEat {
		// Próba podniesienia lewego pałeczki
		sem.Acquire()
		p.leftChopstick.Lock()

		// Próba podniesienia prawego pałeczki
		p.rightChopstick.Lock()

		// Filozof je
		fmt.Printf("Filozof %d je posiłek %d\n", p.id, p.mealsEaten+1)
		p.mealsEaten++

		// Symulacja czasu jedzenia
		time.Sleep(time.Millisecond * 100)

		// Filozof kończy jedzenie
		fmt.Printf("Filozof %d skończył jeść posiłek %d\n", p.id, p.mealsEaten)

		// Zwolnienie pałeczek
		p.leftChopstick.Unlock()
		p.rightChopstick.Unlock()
		sem.Release()
	}
}

type Semaphore chan struct{}

func NewSemaphore(n int) *Semaphore {
	sem := make(Semaphore, n)
	for i := 0; i < n; i++ {
		sem <- struct{}{}
	}
	return &sem
}

func (s *Semaphore) Acquire() {
	<-*s
}

func (s *Semaphore) Release() {
	*s <- struct{}{}
}

func main() {
	// Inicjalizacja pałeczek
	chopsticks := make([]*Chopstick, numPhilosophers)
	for i := 0; i < numPhilosophers; i++ {
		chopsticks[i] = new(Chopstick)
	}

	// Inicjalizacja semafora
	sem := NewSemaphore(numPhilosophers - 1)

	// Inicjalizacja filozofów
	philosophers := make([]*Philosopher, numPhilosophers)
	for i := 0; i < numPhilosophers; i++ {
		philosophers[i] = &Philosopher{
			id:             i + 1,
			leftChopstick:  chopsticks[i],
			rightChopstick: chopsticks[(i+1)%numPhilosophers],
			mealsEaten:     0,
		}
	}

	// Rozpoczęcie posiłków
	var wg sync.WaitGroup
	for _, p := range philosophers {
		wg.Add(1)
		go p.eat(&wg, sem)
	}

	// Oczekiwanie na zakończenie posiłków przez wszystkich filozofów
	wg.Wait()

	// Zakończenie programu
	fmt.Println("Wszyscy filozofowie skończyli jeść")
}
