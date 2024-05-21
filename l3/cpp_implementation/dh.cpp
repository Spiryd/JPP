#include <iostream>
#include <random>
#include <vector>
#include <cmath>
#include <stdexcept>

const long CHARACTERISTIC = 1234567891;
const long ORDER = 1234567891;

bool check(unsigned long suspect) {
    unsigned long i = 2;
    unsigned long tmp = suspect;

    while (i * i <= tmp) {
        if (tmp % i == 0) {
            if (std::pow(suspect, (CHARACTERISTIC - 1) / i) == 1) {
                return false;
            }
            tmp /= i;
        } else {
            i += 1;
        }
    }
    if (tmp > 1 && std::pow(suspect, (CHARACTERISTIC - 1) / tmp) == 1) {
        return false;
    }
    return true;
}

std::vector<int> sieve_of_eratosthenes(int n) {
    std::vector<bool> is_prime(n + 1, true);
    is_prime[0] = false;
    is_prime[1] = false;
    int p = 2;
    while (p * p <= n) {
        if (is_prime[p]) {
            int i = p * p;
            while (i <= n) {
                is_prime[i] = false;
                i += p;
            }
        }
        p += 1;
    }

    std::vector<int> primes;
    for (int i = 0; i <= n; ++i) {
        if (is_prime[i]) {
            primes.push_back(i);
        }
    }
    return primes;
}

std::pair<int, int> power_of_prime(int n) {
    for (int i : sieve_of_eratosthenes(n)) {
        if (n % i == 0) {
            int count = 0;
            while (n % i == 0) {
                n /= i;
                count += 1;
            }
            if (n == 1) {
                return std::make_pair(i, count);
            } else {
                throw std::runtime_error("Not a power of prime.");
            }
        }
    }
    throw std::runtime_error("Not a power of prime.");
}

class Gf {
private:
    unsigned long long value;

    Gf inv() const {
        long long t = 0;
        long long newt = 1;
        long long r = ORDER;
        long long newr = static_cast<long long>(value);
        while (newr != 0) {
            long long quotient = r / newr;
            long long tmp = newt;
            newt = t - quotient * newt;
            t = tmp;
            tmp = newr;
            newr = r - quotient * newr;
            r = tmp;
        }
        if (r > 1) {
            throw std::runtime_error("Value is not invertible.");
        }
        if (t < 0) {
            t += ORDER;
        }
        return Gf(t);
    }

public:
    Gf() : value(0) {}
    Gf(unsigned long long value) : value(value % ORDER) {}

    unsigned long long getValue() const {
        return value;
    }

    unsigned long long characteristic() const {
        auto [prime, _] = power_of_prime(static_cast<int>(ORDER));
        return prime;
    }
    
    friend bool operator==(const Gf& lhs, const Gf& rhs) {
        return lhs.value == rhs.value;
    }

    friend bool operator!=(const Gf& lhs, const Gf& rhs) {
        return !(lhs == rhs);
    }

    friend bool operator<(const Gf& lhs, const Gf& rhs) {
        return lhs.value < rhs.value;
    }

    friend bool operator<=(const Gf& lhs, const Gf& rhs) {
        return lhs.value <= rhs.value;
    }

    friend bool operator>(const Gf& lhs, const Gf& rhs) {
        return lhs.value > rhs.value;
    }

    friend bool operator>=(const Gf& lhs, const Gf& rhs) {
        return lhs.value >= rhs.value;
    }

    friend std::ostream& operator<<(std::ostream& os, const Gf& gf) {
        return os << gf.value;
    }

    Gf operator+(const Gf& other) const {
        return Gf((value + other.value) % ORDER);
    }

    Gf& operator+=(const Gf& other) {
        value = (value + other.value) % ORDER;
        return *this;
    }

    Gf operator-(const Gf& other) const {
        return Gf((value + ORDER - other.value) % ORDER);
    }

    Gf& operator-=(const Gf& other) {
        value = (value + ORDER - other.value) % ORDER;
        return *this;
    }

    Gf operator*(const Gf& other) const {
        return Gf((value * other.value) % ORDER);
    }

    Gf& operator*=(const Gf& other) {
        value = (value * other.value) % ORDER;
        return *this;
    }

    Gf operator/(const Gf& other) const {
        return *this * other.inv();
    }

    Gf& operator/=(const Gf& other) {
        *this *= other.inv();
        return *this;
    }
};

template <class T>
class DHSetup {
private:
    T generator;

public:
    DHSetup() {
        std::random_device rd;
        std::mt19937 rng(rd());
        std::uniform_int_distribution<unsigned long> dist(1, CHARACTERISTIC - 1);

        unsigned long g = dist(rng);
        while (!check(g)) {
            g = dist(rng);
        }
        generator = T(g);
    }

    T getGenerator() {
        return generator;
    }

    T power(T a, unsigned long b) {
        T res = a;
        while (b > 0) {
            if (b % 2 == 1) {
                res = res * a;
            }
            a = a * a;
            b /= 2;
        }
        return res;
    }
};

template <class T>
class User {
private:
    long secret;
    DHSetup<T>* dhsetup;
    T key;

public:
    User(DHSetup<T>* dhsetup) {
        secret = rand();
        this->dhsetup = dhsetup;
        std::cout << "secret: " << secret << std::endl;
    }

    T getPublicKey() {
        return dhsetup->power(dhsetup->getGenerator(), secret);
    }

    void setKey(T a) {
        key = dhsetup->power(a, secret);
        std::cout << "key: " << key.getValue() << std::endl;
    }

    T encrypt(T m) {
        return m * key;
    }

    T decrypt(T c) {
        return c / key;
    }
};

int main() {
    Gf a(11);
    Gf m(2115);

    DHSetup<Gf> dh;
    std::cout << "generator: " << dh.getGenerator().getValue() << std::endl;
    User<Gf> user(&dh);
    Gf pubKey = user.getPublicKey();
    std::cout << "public key: " << pubKey.getValue() << std::endl;
    user.setKey(a);
    std::cout << "message: " << m.getValue() << std::endl;
    Gf c = user.encrypt(m);
    std::cout << "encrypted: " << c.getValue() << std::endl;
    Gf mDecrypted = user.decrypt(c);
    std::cout << "decrypted: " << mDecrypted.getValue() << std::endl;

    return 0;
}