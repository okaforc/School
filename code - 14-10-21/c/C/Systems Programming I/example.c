#include <math.h>
#include <stdio.h>

// initalise methods before using them
float compute_mean(float *nums, int count);
float compute_stdev(float *nums, int count, float mean);

int main(void) {
    const int max_size;
    float nums[max_size];
    float temp;
    int count = 0;

    // get nums from user

    printf("Please enter numbers, ending with -1\n");
    scanf("%f", &temp);
    while (temp != -1) {
        nums[count] = temp;
        count++;
        scanf("%f", &temp);
    }
    float mean = compute_mean(nums, count);
    float stdev = compute_stdev(nums, count, mean);
    printf("The standard dev is %f\n", stdev);
    // we're finished, yay!
    return 0;
}

float compute_stdev(float *nums, int count, float mean) {
    float sum = 0;
    for (int i = 0; i < count; i++) {
        float diff = nums[i] - mean;
        sum = sum + diff * diff;
    }
    return sqrt(sum);
}

float compute_mean(float *nums, int count) {
    float sum = 0;
    for (int i = 0; i < count; i++) {
        sum += nums[i];
    }
    return sum / ((float)count);
}
