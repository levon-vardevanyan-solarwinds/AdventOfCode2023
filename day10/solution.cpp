#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <set>
#include <string>
#include <vector>
#include <stdexcept>

using Grid = std::vector<std::vector<char>>;

struct Coord
{
    long i = 0;
    long j = 0;

    friend bool operator<(Coord const& lhs, Coord const& rhs)
    {
        return std::tie(lhs.i, lhs.j) < std::tie(rhs.i, rhs.j);
    }
};

using Path = std::vector<Coord>;

//    | is a vertical pipe connecting north and south.
//    - is a horizontal pipe connecting east and west.
//    L is a 90-degree bend connecting north and east.
//    J is a 90-degree bend connecting north and west.
//    7 is a 90-degree bend connecting south and west.
//    F is a 90-degree bend connecting south and east.
//    . is ground; there is no pipe in this tile.
bool connected(Grid const& board, Coord const& from, Coord const& to)
{
    // assuming that start is connected to all directions
    std::set<char> const north_connected{'S', '|', 'L', 'J'};
    std::set<char> const south_connected{'S', '|', '7', 'F'};
    std::set<char> const west_connected{'S', '-', 'J', '7'};
    std::set<char> const east_connected{'S', '-', 'L', 'F'};

    // NORTH
    if (from.j == to.j && from.i - 1 == to.i)
    {
        return north_connected.contains(board[from.i][from.j]) &&
               south_connected.contains(board[to.i][to.j]);
    }

    // SOUTH
    if (from.j == to.j && from.i + 1 == to.i)
    {
        return south_connected.contains(board[from.i][from.j]) &&
               north_connected.contains(board[to.i][to.j]);
    }

    // WEST
    if (from.i == to.i && from.j - 1 == to.j)
    {
        return west_connected.contains(board[from.i][from.j]) &&
               east_connected.contains(board[to.i][to.j]);
    }

    // EAST
    if (from.i == to.i && from.j + 1 == to.j)
    {
        return east_connected.contains(board[from.i][from.j]) &&
               west_connected.contains(board[to.i][to.j]);
    }

    throw std::invalid_argument("unreachable");
}

std::vector<Coord> adjacents(Coord const& coord)
{
    std::vector<Coord> result;

    result.push_back(Coord{coord.i - 1, coord.j}); // NORTH
    result.push_back(Coord{coord.i, coord.j - 1}); // WEST
    result.push_back(Coord{coord.i, coord.j + 1}); // EAST
    result.push_back(Coord{coord.i + 1, coord.j}); // SOUTH

    return result;
}

std::vector<Coord> next_steps(Grid const& board, Coord const& from, std::set<Coord> const& visited)
{
    auto options = adjacents(from);
    for (auto it = options.begin(); it != options.end();)
    {
        // remove from results if out of bounds, if already visited or can't connect the pipe
        if (it->i < 0 || it->j >= board.size() || visited.contains(*it) || !connected(board, from, *it))
        {
            it = options.erase(it);
        }
        else
        {
            ++it;
        }
    }

    return options;
}

std::vector<Path> result;

void crawl(Grid const& board, Coord const& from, Path path, std::set<Coord> visited)
{
    visited.insert(from);

    auto const steps = next_steps(board, from, visited);
    if (steps.empty())
    {
        std::cout << "result = " << path.size() / 2 + 1 << std::endl;
        result.push_back(path);
    }

    for (auto const& c : steps)
    {
        Path p(path);
        p.push_back(c);
        crawl(board, c, p, visited);
    }
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

    Coord start;

    for (int i = 0; i < board.size(); ++i)
    {
        for (int j = 0; j < board.front().size(); ++j)
        {
            if (board[i][j] == 'S')
            {
                start = {i, j};
            }
        }
    }

    crawl(board, start, { }, { });
    std::cout << result.size() << std::endl;

    auto const& backwards = result.back();
    std::ofstream output("output.txt");
    for (int k = 0; k < backwards.size(); ++k)
    {
        auto const& c = backwards[k];
        output << c.i << ' ' << c.j << '\n';
    }
}

