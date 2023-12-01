#include <algorithm>
#include <cctype>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

int char_digit_to_int(char ch)
{
    return ch - '0';
}

int main()
{
    auto is_digit = [](unsigned char c)
    {
        return std::isdigit(c);
    };

    auto part_one = [is_digit]
    {
        std::ifstream input("input.txt");

        int result = 0;

        for (std::string line; std::getline(input, line); )
        {
            auto first_digit = std::find_if(line.begin(), line.end(), is_digit);
            auto last_digit = std::find_if(line.rbegin(), line.rend(), is_digit);

            result += char_digit_to_int(*first_digit) * 10 + char_digit_to_int(*last_digit);
        }

        return result;
    };

    std::cout << "Part one solution is " << part_one() << '\n';


    auto part_two = [is_digit]
    {
        std::vector<std::string> const digits{"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
        std::vector<std::string> rdigits(digits);
        std::for_each(rdigits.begin(), rdigits.end(), [](auto& x) { std::reverse(x.begin(), x.end()); });

        std::ifstream input("input.txt");

        int result = 0;

        for (std::string line; std::getline(input, line); )
        {
            auto search_digit = [&line, is_digit](auto const& digits)
            {
                auto first_it = std::find_if(line.begin(), line.end(), is_digit);
                int first_digit = char_digit_to_int(*first_it);

                for (int i = 0; i < digits.size(); ++i)
                {
                    auto const& digit = digits[i];
                    auto it = std::search(line.begin(), line.end(), digit.begin(), digit.end());
                    if (it != line.end() &&
                        std::distance(line.begin(), it) < std::distance(line.begin(), first_it))
                    {
                        first_it = it;
                        first_digit = i + 1;
                    }
                }

                return first_digit;
            };

            int const first_digit = search_digit(digits);

            std::reverse(line.begin(), line.end());

            int const last_digit = search_digit(rdigits);

            result += first_digit * 10 + last_digit;
        }

        return result;
    };

    std::cout << "Part two solution is " << part_two() << '\n';
}
