#include <stdio.h>
#include <math.h>

long long primeFactor(long long test);

int main() {
    primeFactor(600851475143);
};

long long isAFactor(long long number, long long factor){
    if(number % factor == 0){
        return 1;
    } else {
        return 0;
    }
};

long long isPrime(long long prime){
    for(long long q; q <= sqrt(prime); q++){
        if(prime % q == 0){
            return 0;
        };
    };
    return 1;
};

long long primeFactor(long long test){
    long long largestPrimeFactor = 1;
    for(long long c = 1; c < test/2; c++){
        if(isPrime(c) && isAFactor(c, test)){
            largestPrimeFactor = c;
            printf("%lld\n", largestPrimeFactor);
        };
    };
    return largestPrimeFactor;
};
