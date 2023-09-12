#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int removeDuplicates(int* nums, int numsSize) {
    if (!numsSize) return 0;

    int nonDup = 1;
    int lastValid = 0; // index of last valid number

    for (int i = 1; i < numsSize; i++) {
        if (nums[i] != nums[i-1]) {
            lastValid++;
            nonDup++;
            nums[lastValid] = nums[i];
        }
    }
    return nonDup;
}

int main(int argc, char const *argv[]) {
    int nums[] = { 0,0,1,1,1,2,2,3,3,4 };
    printf("%d\n", removeDuplicates(nums, 10));
    for(int i = 0; i < 10; i++) {
        printf("%d, ", nums[i]);
    }
    return 0;
}
