#include <vector>
#include <iostream>

std::vector<int> decimal_to_binary(int decimal)
{
    if (decimal == 0)
        return {0};

    std::vector<int> results{};
    while (decimal != 0)
    {
        results.push_back(decimal % 2);
        decimal /= 2;
    }
    std::reverse(results.begin(), results.end());

    return results;
}

int binary_to_decimal(std::vector<int> binary)
{
    int res = 0;
    int n = binary.size();
    for (int i = 0; i < n; ++i)
    {
        res += binary[n - 1 - i] * std::pow(2, i);
    }
    return res;
}

int main(void)
{
    std::vector<int> res = decimal_to_binary (13);
    for (const int& elem : res){
        std::cout<<elem<<'\t';
    }
    return 0;
}