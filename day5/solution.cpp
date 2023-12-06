#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <sstream>
#include <thread>
#include <vector>

struct Range
{
    Range(std::vector<long> const& data) :
        dest_start(data[0]),
        source_start(data[1]),
        length(data[2])
    {

    }

    long dest_start;
    long source_start;
    long length;
};

using Dict = std::vector<Range>;

long translate(long x, Dict const& dict)
{
    for (auto const& range : dict)
    {
        if (x >= range.source_start && x < range.source_start + range.length)
        {
            return range.dest_start + (x - range.source_start);
        }
    }

    return x;
}

std::vector<long> line_to_numbers(std::string const& str)
{
    std::vector<long> results;

    std::istringstream tokenizer(str);
    for (long x = 0; tokenizer >> x;)
    {
        results.push_back(x);
    }

    return results;
}

std::vector<Dict> parse_lists(std::istream& input)
{
    std::vector<Dict> result;

    for (;;)
    {
        std::string title;
        if (!std::getline(input, title))
        {
            break;
        }

        Dict dict;

        std::string line;
        while (std::getline(input, line))
        {
            if (line.empty())
            {
                break;
            }

            auto const numbers = line_to_numbers(line);
            dict.push_back(Range(numbers));
        }

        result.push_back(dict);
    }

    return result;
}

int main()
{
    std::vector<long> const seeds
    {
        950527520,  85181200,
        546703948,  123777711,
        63627802,   279111951,
        1141059215, 246466925,
        1655973293, 98210926,
        3948361820, 92804510,
        2424412143, 247735408,
        4140139679, 82572647,
        2009732824, 325159757,
        3575518161, 370114248
    };

    std::vector<Dict> dicts;
    {
        std::ifstream input("input.txt");
        dicts = parse_lists(input);
    }

    auto yield = [&dicts](long seed, long& result)
    {
        auto x = seed;

        for (auto const& dict : dicts)
        {
            x = translate(x, dict);
        }

        result = std::min(result, x);
    };

    constexpr auto MAX_LONG = std::numeric_limits<long>::max();

    long result = MAX_LONG;

    for (auto& seed : seeds)
    {
        yield(seed, result);
    }

    std::cout << result << std::endl;

    std::vector<long> results(seeds.size() / 2, MAX_LONG);

    std::vector<std::thread> threads;

    for (auto i = 0u; i < results.size(); ++i)
    {
        threads.emplace_back([i, yield, &seeds, &results]()
        {
            auto const from = seeds[i * 2];
            auto const to = from + seeds[i * 2 + 1];
            for (auto j = from; j < to; ++j)
            {
                yield(j, results[i]);
            }
        });
    }

    for (auto& thread : threads)
    {
        thread.join();
    }

    std::cout << *std::min_element(results.begin(), results.end()) << std::endl;
}

