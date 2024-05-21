import java.math.BigInteger;
import java.util.Random;
import java.util.function.Function;

import java.util.ArrayList;
import java.util.List;

public class Main {

    public static void main(String[] args) {
        Gf a = new Gf(11);
        Gf m = new Gf(2115);

        DHSetup<Gf> dh = new DHSetup<>(Gf::new);
        System.out.println("generator: " + dh.getGenerator());
        User<Gf> user = new User<>(dh);
        Gf pubKey = user.getPublicKey();
        System.out.println("public key: " + pubKey);
        user.setKey(a);
        System.out.println("message: " + m);
        Gf c = user.encrypt(m);
        System.out.println("encrypted: " + c);
        Gf mDecrypted = user.decrypt(c);
        System.out.println("decrypted: " + mDecrypted);
    }

    private static final long CHARACTERISTIC = 1234577;

    public interface AlgebraicBody {
        AlgebraicBody multiply(AlgebraicBody other);
        AlgebraicBody divide(AlgebraicBody other);
        AlgebraicBody fromLong(long value);
    }

    public static boolean check(long suspect) {
        long i = 2;
        long tmp = suspect;
        while (i * i <= tmp) {
            if (tmp % i == 0) {
                if (BigInteger.valueOf(suspect).pow((int)((CHARACTERISTIC - 1) / i)).mod(BigInteger.valueOf(CHARACTERISTIC)).equals(BigInteger.ONE)) {
                    return false;
                }
                tmp /= i;
            } else {
                i += 1;
            }
        }
        if (tmp > 1 && BigInteger.valueOf(suspect).pow((int)((CHARACTERISTIC - 1) / tmp)).mod(BigInteger.valueOf(CHARACTERISTIC)).equals(BigInteger.ONE)) {
            return false;
        }
        return true;
    }

    public static class DHSetup<T extends AlgebraicBody> {
        private T generator;

        public DHSetup(Function<Long, T> constructor) {
            Random rng = new Random();
            long generatorValue = rng.nextInt((int) CHARACTERISTIC - 1) + 1;
            while (!check(generatorValue)) {
                generatorValue = rng.nextInt((int) CHARACTERISTIC - 1) + 1;
            }
            this.generator = constructor.apply(generatorValue);
        }

        public T getGenerator() {
            return generator;
        }

        public T power(T a, long b) {
            T res = a;
            while (b > 0) {
                if (b % 2 == 1) {
                    res = (T) res.multiply(a);
                }
                a = (T) a.multiply(a);
                b /= 2;
            }
            return res;
        }
    }

    public static class User<T extends AlgebraicBody> {
        private long secret;
        private DHSetup<T> dhsetup;
        private T key;

        public User(DHSetup<T> dhsetup) {
            this.dhsetup = dhsetup;
            this.secret = new Random().nextLong();
            System.out.println("secret: " + secret);
        }

        public T getPublicKey() {
            return dhsetup.power(dhsetup.getGenerator(), secret);
        }

        public void setKey(T a) {
            this.key = dhsetup.power(a, secret);
            System.out.println("key: " + key);
        }

        public T encrypt(T m) {
            return (T) m.multiply(key);
        }
    
        public T decrypt(T c) {
            return (T) c.divide(key);
        }
    }
}

class Gf implements Main.AlgebraicBody {
    private static final long ORDER = 1234577;
    private long value;

    public Gf(long value) {
        this.value = value % ORDER;
    }

    public long getValue() {
        return value;
    }

    public long characteristic() {
        long[] primePower = powerOfPrime(ORDER);
        return primePower[0];
    }

    private Gf inv() {
        long t = 0;
        long newt = 1;
        long r = ORDER;
        long newr = value;
        while (newr != 0) {
            long quotient = r / newr;
            long tmp = newt;
            newt = t - quotient * newt;
            t = tmp;
            tmp = newr;
            newr = r - quotient * newr;
            r = tmp;
        }
        if (r > 1) {
            throw new ArithmeticException(value + " is not invertible");
        }
        if (t < 0) {
            t += ORDER;
        }
        return new Gf(t);
    }

    public boolean equals(Gf other) {
        return value == other.value;
    }

    public boolean notEquals(Gf other) {
        return !equals(other);
    }

    public boolean lessThanOrEqual(Gf other) {
        return value <= other.value;
    }

    public boolean greaterThanOrEqual(Gf other) {
        return value >= other.value;
    }

    public boolean lessThan(Gf other) {
        return value < other.value;
    }

    public boolean greaterThan(Gf other) {
        return value > other.value;
    }

    public Gf add(Gf other) {
        return new Gf((value + other.value) % ORDER);
    }

    public void addAssign(Gf other) {
        value = (value + other.value) % ORDER;
    }

    public Gf subtract(Gf other) {
        return new Gf((value + ORDER - other.value) % ORDER);
    }

    public void subtractAssign(Gf other) {
        value = (value + ORDER - other.value) % ORDER;
    }
    
    public Gf multiply(Gf other) {
        return new Gf((value * other.value) % ORDER);
    }

    public void multiplyAssign(Gf other) {
        value = (value * other.value) % ORDER;
    }

    public Gf divide(Gf other) {
        return multiply(other.inv());
    }

    public void divideAssign(Gf other) {
        multiplyAssign(other.inv());
    }

    private static long[] powerOfPrime(long n) {
        for (long i : sieveOfEratosthenes(n)) {
            if (n % i == 0) {
                long count = 0;
                while (n % i == 0) {
                    n /= i;
                    count++;
                }
                if (n == 1) {
                    return new long[]{i, count};
                } else {
                    throw new ArithmeticException("Not a power of prime");
                }
            }
        }
        throw new ArithmeticException("Not a power of prime");
    }

    private static List<Long> sieveOfEratosthenes(long n) {
        List<Boolean> isPrime = new ArrayList<>();
        for (int i = 0; i <= n; i++) {
            isPrime.add(true);
        }
        isPrime.set(0, false);
        isPrime.set(1, false);
        int p = 2;
        while (p * p <= n) {
            if (isPrime.get(p)) {
                int i = p * p;
                while (i <= n) {
                    isPrime.set(i, false);
                    i += p;
                }
            }
            p++;
        }
        List<Long> primes = new ArrayList<>();
        for (int i = 0; i <= n; i++) {
            if (isPrime.get(i)) {
                primes.add((long) i);
            }
        }
        return primes;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    @Override
    public Main.AlgebraicBody multiply(Main.AlgebraicBody other) {
        return multiply((Gf) other);
    }

    @Override
    public Main.AlgebraicBody divide(Main.AlgebraicBody other) {
        return divide((Gf) other);
    }

    @Override
    public Main.AlgebraicBody fromLong(long value) {
        return new Gf(value);
    }
}

