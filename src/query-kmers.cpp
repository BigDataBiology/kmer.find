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

class int_range {
    private:
        const uint32_t * cur_;
        const uint32_t * past_;

    public:
        int_range(const uint32_t* begin, const uint32_t* end)
            :cur_(begin)
            ,past_(end) { }
        uint32_t top() const { return *cur_; }
        void pop() { ++cur_; }
        bool empty() const { return cur_ == past_; }

        bool operator < (const int_range& lhs) const { return top() > lhs.top(); }
};


void maybe_repush(std::priority_queue<int_range>& q) {
    int_range r = q.top();
    q.pop();
    r.pop();
    if (!r.empty()) q.push(r);
}

int main(int argc, char* argv[]) {
    struct stat st1, st2;
    int fd1 = open(argv[1], O_RDONLY);
    int fd2 = open(argv[2], O_RDONLY);
    if (fd1 < 0) {
        std::cerr << "Failed to open " << argv[1] << '\n';
        return 1;
    }
    if (fd2 < 0) {
        std::cerr << "Failed to open " << argv[1] << '\n';
        return 1;
    }
    if (fstat(fd1, &st1)) {
        std::cerr << "Failed to fstat on " << argv[1] << '\n';
        return 1;
    }
    if (fstat(fd2, &st2)) {
        std::cerr << "Failed to fstat on " << argv[2] << '\n';
        return 1;
    }

    const uint64_t* ix1 = static_cast<const uint64_t*>(mmap(0, st1.st_size, PROT_READ, MAP_PRIVATE, fd1, 0));
    const uint32_t* ix2 = static_cast<const uint32_t*>(mmap(0, st2.st_size, PROT_READ, MAP_PRIVATE, fd2, 0));

    if (ix1 == MAP_FAILED || ix2 == MAP_FAILED) {
        std::cerr << "mmap() failed\n";
        return 1;
    }

    std::string header;
    std::string seq;
    while (std::getline(std::cin, header) && std::getline(std::cin, seq)) {
        std::priority_queue<int_range> q;
        for (unsigned i = 0; i != seq.length() - KMER_SIZE; ++i) {
            unsigned kmer = compute_kmer(seq, i);
            int_range r(ix2 + ix1[kmer], ix2 + ix1[kmer + 1]);
            if (!r.empty()) {
                q.push(r);
            }
        }
        std::vector<std::pair<int, uint32_t>> counts;

        while (!q.empty()) {
            uint32_t cur = q.top().top();
            int c = 1;
            maybe_repush(q);
            while (!q.empty() && cur == q.top().top()) {
                ++c;
                maybe_repush(q);
            }
            counts.push_back(std::make_pair(c, cur));
        }
        std::sort(counts.begin(), counts.end());
        const unsigned n = (counts.size() < 100 ? counts.size() : 100);
        std::cout << header << '\n';
        for (unsigned i = 0; i != n; ++i) {
            std::cout << counts[counts.size() - 1 - i].second << '\n';
        }
    }

    munmap(const_cast<void*>(static_cast<const void*>(ix1)), st1.st_size);
    munmap(const_cast<void*>(static_cast<const void*>(ix2)), st2.st_size);
    close(fd1);
    close(fd2);
}
