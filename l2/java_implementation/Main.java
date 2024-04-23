import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        Gf a = new Gf(10);
        Gf b = new Gf(5);
        System.out.println("a = " + a);
        System.out.println("b = " + b);
        System.out.println("a + b = " + a.add(b));
        System.out.println("a * b = " + a.multiply(b));
        System.out.println("a / b = " + a.divide(b));
        System.out.println("characteristic = " + a.characteristic());
    }
}

class Gf {
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
}

