#include <iostream>
#include <fstream>
#include <vector>
#include <cinttypes>

#include <sys/types.h>
#include <sys/stat.h>

#include <fcntl.h>
#include <unistd.h>

template<typename T>
class obuffer {
    std::vector<T> data_;
    int fd_;
    const unsigned size_;
    public:

    obuffer(const char* fname, unsigned size)
        :size_(size)
    {
        fd_ = open(fname, O_WRONLY | O_CREAT, 0644);
        if (fd_ < 0) {
            std::cerr << "Failed to open " << fd_ << '\n';
            throw 1;
        }
        data_.reserve(size);
    }

    ~obuffer() {
        this->flush();
        close(fd_);
    }

    void push(T t) {
        data_.push_back(t);
        if (data_.size() == size_) this->flush();
    }


    void flush() {
        const unsigned len = data_.size() * sizeof(T);
        if (write(fd_, data_.data(), len) != len) {
            std::cerr << "Failed to write!\n";
            throw 1;
        }
        data_.clear();
    }

};


int main(int argc, char* argv[]) {
    if (argc < 4) {
        std::cerr << "Wrong number of arguments\n";
        return 1;
    }
    std::ifstream fin(argv[1], std::ios::binary);
    obuffer<uint64_t> ix1(argv[2], 4096);
    obuffer<uint32_t> ix2(argv[3], 4096);
    uint32_t ibuf[1024 * 2];

    uint32_t ci = 0;
    uint64_t pos = 0;
    ix1.push(pos);
    while (true) {
        fin.read((char*)ibuf, sizeof(ibuf));
        const unsigned nbytes = fin.gcount();
        if (!nbytes) break;

        if (nbytes % (2 * sizeof(ibuf[0])) != 0) {
            std::cerr << "Bad input file " << argv[1]
                        << " size is incorrect (read " << nbytes
                        << " Bytes; not multiple of " << (2*sizeof(ibuf[0])) << ")."
                        << std::endl;
            throw "Bad input";
        }
        const int n_pairs = nbytes/(2 * sizeof(ibuf[0]));
        for (int i = 0; i < n_pairs; ++i) {
            uint32_t k = ibuf[2*i];
            ix2.push(ibuf[2*i + 1]);
            while (ci < k) {
                ++ci;
                ix1.push(pos);
            }
            ++pos;
        }
    }
    const uint32_t final_k = 1 << 29;
    while (ci < final_k) {
        ++ci;
        ix1.push(pos);
    }
    ix1.flush();
    ix2.flush();
}
