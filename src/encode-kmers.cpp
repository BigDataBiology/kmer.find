#include <iostream>
#include <algorithm>
#include <queue>
#include <vector>
#include <cinttypes>

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include "kmers.h"

const char* encode(const int i) {
    const char* numbers[] = { "0", "1", "2", "3", "4", "5",
        "6", "7", "8", "9", "10", "11", "12", "13", "14", "15" };
    return numbers[i];
}

int main(int argc, char* argv[]) {
    std::vector<int> fdouts;
    std::vector<std::vector<uint32_t>> buffers;
    for (int i = 0; i != 16; ++i) {
        int f = open((std::string(argv[1]) + "." + encode(i)).c_str(), O_WRONLY | O_CREAT, 0644);
        if (f < 0) {
            std::cerr << "Failed to open " << argv[1] << '\n';
            return 1;
        }
        fdouts.push_back(f);
        buffers.push_back(std::vector<uint32_t>());
        buffers.back().reserve(4096);
    }
    uint32_t ix = 0;
    std::string header;
    std::string seq;
    while (std::getline(std::cin, header) && std::getline(std::cin, seq)) {
        for (unsigned i = 0; i != seq.length() - KMER_SIZE + 1; ++i) {
            const uint32_t kmer = compute_kmer(seq, i);
            const unsigned fix = kmer >> 24;
            buffers[fix].push_back(kmer);
            buffers[fix].push_back(ix);
            if (buffers[fix].size() == 4096) {
                const unsigned len = buffers[fix].size() * sizeof(uint32_t);
                if (write(fdouts.at(fix), buffers[fix].data(), len) != len) {
                    std::cerr << "Failed to write!\n";
                    return 1;
                }
                buffers[fix].clear();
            }
        }
        ++ix;
    }
    for (int i = 0; i != 16; ++i) {
        if (!buffers[i].empty()) {
                const unsigned len = buffers[i].size() * sizeof(uint32_t);
                if (write(fdouts.at(i), buffers[i].data(), len) != len) {
                    std::cerr << "Failed to write (last chunk)!\n";
                    return 1;
                }
        }
        close(fdouts[i]);
    }
}

