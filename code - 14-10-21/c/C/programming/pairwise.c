#include <math.h>
#include <stdio.h>


float pairwise_sum(const float *array, int size);

int main(int argc, char const *argv[]) {
    float test[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    float t = pairwise_sum(test, 10);
    printf("%.0f", t);
    return 0;
}

float pairwise_sum(const float *array, int size) {
    float sum;
    int i = 0, j = 0;
    float aux[size / 2];

    if (size == 1) {
        return array[0];
    }

    if (size == 2) {
        return array[0] + array[1];
    }

    for (i = 0; i < size; i += 2) {
        if (i == size - 1) {
            aux[(size / 2) - 1] += array[i];
        } else {
            aux[j] = array[i] + array[i + 1];
        }
        j++;
    }

    sum = pairwise_sum(aux, size / 2);

    return sum;
}