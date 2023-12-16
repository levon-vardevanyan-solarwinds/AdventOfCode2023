#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <queue>
#include <set>
#include <map>
#include <string>
#include <vector>

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

enum class Direction
{
    NORTH, WEST, EAST, SOUTH
};

struct Ray
{
    Coord from;
    Direction direction;

    friend bool operator<(Ray const& lhs, Ray const& rhs)
    {
        return std::tie(lhs.from, lhs.direction) < std::tie(rhs.from, rhs.direction);
    }
};

Coord next_step(Coord const& from, Direction direction)
{
    switch (direction)
    {
        case Direction::NORTH:
            return Coord{from.i - 1, from.j};
        case Direction::SOUTH:
            return Coord{from.i + 1, from.j};
        case Direction::WEST:
            return Coord{from.i, from.j - 1};
        case Direction::EAST:
            return Coord{from.i, from.j + 1};
    };
}

void cast(Ray const& ray, Grid const& board, Grid& result,
        std::queue<Ray>& to_process, std::set<Ray>& visited)
{
    auto const N = result.size();
    auto const M = result.front().size();

    auto out_bounds = [N,M](Coord const& k)
    {
        return k.i < 0 || k.j < 0 || k.i >= N || k.j >= M;
    };

    for (auto k = ray.from; !out_bounds(k); k = next_step(k, ray.direction))
    {
        result[k.i][k.j] = '#';

        auto ch = board[k.i][k.j];
        if (ch == '\\' || ch == '/' || ch == '|' || ch == '-')
        {
            auto node = Ray{k, ray.direction};
            if (visited.contains(node))
            {
                break;
            }
            else
            {
                visited.insert(std::move(node));
            }

            // mirrors
            if (ch == '\\')
            {
                static std::map<Direction, Direction> const dict
                {
                    { Direction::NORTH, Direction::WEST },
                    { Direction::WEST, Direction::NORTH },
                    { Direction::SOUTH, Direction::EAST },
                    { Direction::EAST, Direction::SOUTH },
                };
                Direction const new_direction = dict.at(ray.direction);
                to_process.push(Ray{next_step(k, new_direction), new_direction});
                break;
            }
            if (ch == '/')
            {
                static std::map<Direction, Direction> const dict
                {
                    { Direction::NORTH, Direction::EAST },
                    { Direction::EAST, Direction::NORTH },
                    { Direction::SOUTH, Direction::WEST },
                    { Direction::WEST, Direction::SOUTH },
                };
                Direction const new_direction = dict.at(ray.direction);
                to_process.push(Ray{next_step(k, new_direction), new_direction});
                break;
            }
            // splitters
            if (ch == '|' && (ray.direction == Direction::WEST ||
                              ray.direction == Direction::EAST)) 
            {
                to_process.push(Ray{next_step(k, Direction::SOUTH), Direction::SOUTH});
                to_process.push(Ray{next_step(k, Direction::NORTH), Direction::NORTH});
                break;
            }
            if (ch == '-' && (ray.direction == Direction::SOUTH ||
                              ray.direction == Direction::NORTH))
            {
                to_process.push(Ray{next_step(k, Direction::WEST), Direction::WEST});
                to_process.push(Ray{next_step(k, Direction::EAST), Direction::EAST});
                break;
            }
        }

    }
}

long solve(Grid const&  board, Ray ray)
{
    std::queue<Ray> to_process;
    to_process.push(std::move(ray));

    Grid result(board);

    std::set<Ray> visited;

    while (!to_process.empty())
    {
        auto const& ray = to_process.front();
        cast(ray, board, result, to_process, visited);
        to_process.pop();
    }

    long sum = 0;
    for (int i = 0; i < result.size(); ++i)
    {
        for (int j = 0; j < result.front().size(); ++j)
        {
            if (result[i][j] == '#')
            {
                ++sum;
            }
        }
    }

    return sum;
}

int main()
{
    Grid board;
    {
        std::ifstream input("input.txt");
        for (std::string line; std::getline(input, line); )
        {
            board.emplace_back(line.begin(), line.end());
        }
    }

    std::cout << solve(board, Ray{Coord{0, 0}, Direction::EAST}) << std::endl;

    long const N = board.size();
    long const M = board.front().size();

    long max_sum = 0;
    for (int i = 0; i < N; ++i)
    {
        max_sum = std::max(max_sum, solve(board, Ray{Coord{i, 0}, Direction::EAST}));
        max_sum = std::max(max_sum, solve(board, Ray{Coord{i, M - 1}, Direction::WEST}));
    }

    for (int j = 0; j < M; ++j)
    {
        max_sum = std::max(max_sum, solve(board, Ray{Coord{0, j}, Direction::SOUTH}));
        max_sum = std::max(max_sum, solve(board, Ray{Coord{N - 1, j}, Direction::NORTH}));
    }

    std::cout << max_sum << std::endl;
}

