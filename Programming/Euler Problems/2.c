#include <stdio.h>

int fibonnaciSum(int i);

int main() {
    fibonnaciSum(4000000);
};

int fibonnaciSum(int i){
    int fib = 0;
    int fibAdder1 = 1;
    int fibAdder2 = 0;
    int sum = 0;
    for (int c = 0; fib < i; c++){
        fibAdder2 = fib;
        fib = fibAdder1 + fibAdder2;
        if(fib < i && fib % 2 == 0){
            sum = sum + fib;
        };
        fibAdder1 = fib;
        fib = fibAdder1 + fibAdder2;
        if(fib < i && fib % 2 == 0){
            sum = sum + fib;
        };
    };
    printf("%d\n", sum);
    return sum;
};
