/*
 * this code labels the vowels in an interaction like this
 * enter a word
 * potato
 * p :
 * o :V
 * t :
 * a :V
 * t :
 * o :V
 */

#include <iostream>
#include <string>
using namespace std;

// declaration before use, implementation will actually be later in the file
bool is_vowel(char c);
bool is_const(char c);

int main() {

    string s;
    cout << "enter a word\n";
    cin >> s;
    for (int i = 0; i < s.size(); i++) {
        char c = s[i], result = ' ';
		if (is_vowel(c)) result = 'V';
		else result = 'C';
		cout << c << ": " << result << "\n";
    }

    // loop thru characters of s
    // make each output depend on what is_vowel(..) says
}

// write a function which returns true of input char is
// a e i o u
bool is_vowel(char c) {
    if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') {
        return true;
    }
    return false;
}
