#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <vector>

struct Coord
{
    long i = 0;
    long j = 0;

    friend bool operator<(Coord const& lhs, Coord const& rhs)
    {
        return std::tie(lhs.i, lhs.j) < std::tie(rhs.i, rhs.j);
    }
};

struct Range
{
    Coord from;
    Coord to;
};

struct Record
{
    int value;
    Range range;
};

bool is_digit(unsigned char c)
{
    return std::isdigit(c);
};

std::set<Coord> adjacents(Range const& range)
{
    Coord const NW{ range.from.i - 1, range.from.j - 1 };
    Coord const SW{ range.from.i + 1, range.from.j - 1 };
    Coord const NE{ range.to.i - 1, range.to.j + 1 };
    Coord const SE{ range.to.i + 1, range.to.j + 1 };

    std::set<Coord> result;
    // NW -> NE
    for (int j = NW.j; j <= NE.j; ++j)
    {
        result.insert(Coord{NW.i, j});
    }
    // SW -> SE
    for (int j = SW.j; j <= SE.j; ++j)
    {
        result.insert(Coord{SW.i, j});
    }
    // NW -> SW
    for (int i = NW.i; i <= SW.i; ++i)
    {
        result.insert(Coord{i, NW.j});
    }
    // NE -> SE
    for (int i = NE.i; i <= SE.i; ++i)
    {
        result.insert(Coord{i, NE.j});
    }

    return result;
}

int main()
{
    std::vector<std::vector<char>> board;
    {
        std::ifstream input("input.txt");
        for (std::string line; std::getline(input, line); )
        {
            board.emplace_back(line.begin(), line.end());
        }
    }

    std::vector<Record> numbers;
    for (int i = 0; i < board.size(); ++i)
    {
        auto const& line = board[i];

        auto from = line.begin();
        auto to = from;
        for (;; from = to)
        {
            from = std::find_if(from, line.end(), is_digit);
            if (from == line.end())
            {
                break;
            }
            
            to = std::find_if_not(from, line.end(), is_digit);
            Range range
            {
                Coord{i, std::distance(line.begin(), from)},
                Coord{i, std::distance(line.begin(), to) - 1}
            };
            numbers.push_back(Record{std::stoi(std::string(from, to)), range});
        }
    }

    long result = 0;
    std::map<Coord, std::vector<int>> gears;

    for (auto const& number : numbers)
    {
        auto const neighbours = adjacents(number.range);
        bool symbol_found = false;
        for (auto const& neighbour : neighbours)
        {
            if (neighbour.i < 0 || neighbour.j < 0 ||
                neighbour.i >= board.size() ||
                neighbour.j >= board.front().size())
            {
                continue;
            }

            auto const ch = board[neighbour.i][neighbour.j];
            if (ch != '.')
            {
                symbol_found = true;
                if (ch == '*')
                {
                    auto it = gears.find(neighbour);
                    if (it == gears.end())
                    {
                        gears[neighbour] = std::vector<int>{ number.value };
                    }
                    else
                    {
                        it->second.push_back(number.value);
                    }
                }
            }
        }

        if (symbol_found)
        {
            result += number.value;
        }
    }

    std::cout << result << std::endl;

    long result2 = 0;

    for (auto const& [key, value] : gears)
    {
        if (value.size() == 2)
        {
            result2 += value[0] * value[1];
        }
    }

    std::cout << result2 << std::endl;
}

