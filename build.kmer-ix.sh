#!/usr/bin/env bash

set -e
export PATH=$PWD/bin/:$PATH

input_faa="$1"

index_dir=kmer.index

if test -d ${index_dir}; then
    echo "Index directory (${index_dir}) already exists. Exiting"
    exit 1
fi

if ! test -f ${input_faa}.dhi; then
	echo "FASTA file not indexed. Indexing..."
	index_fasta ${input_faa}
fi


mkdir ${index_dir}
echo "starting..."
python fasta_pre_process.py ${input_faa} /dev/stdout | encode-kmers ${index_dir}/${input_faa}.kmer.ix

echo "Encoding kmers done"
python extract-names32.py ${input_faa} ${index_dir}/${input_faa}.names.32
cd ${index_dir}
du -sh *

for k in *.kmer.ix.*; do
    sort32pairs $k ${k}.sorted
    rm ${k}
    echo "Sorted ${k}"
done

cat ${input_faa}.kmer.ix.0.sorted \
    ${input_faa}.kmer.ix.1.sorted \
    ${input_faa}.kmer.ix.2.sorted \
    ${input_faa}.kmer.ix.3.sorted \
    ${input_faa}.kmer.ix.4.sorted \
    ${input_faa}.kmer.ix.5.sorted \
    ${input_faa}.kmer.ix.6.sorted \
    ${input_faa}.kmer.ix.7.sorted \
    ${input_faa}.kmer.ix.8.sorted \
    ${input_faa}.kmer.ix.9.sorted \
    ${input_faa}.kmer.ix.10.sorted \
    ${input_faa}.kmer.ix.11.sorted \
    ${input_faa}.kmer.ix.12.sorted \
    ${input_faa}.kmer.ix.13.sorted \
    ${input_faa}.kmer.ix.14.sorted \
    ${input_faa}.kmer.ix.15.sorted \
        | build-index2 /dev/stdin ${input_faa}.kmer.ix1 ${input_faa}.kmer.ix2

rm  ${input_faa}.kmer.ix.0.sorted \
    ${input_faa}.kmer.ix.1.sorted \
    ${input_faa}.kmer.ix.2.sorted \
    ${input_faa}.kmer.ix.3.sorted \
    ${input_faa}.kmer.ix.4.sorted \
    ${input_faa}.kmer.ix.5.sorted \
    ${input_faa}.kmer.ix.6.sorted \
    ${input_faa}.kmer.ix.7.sorted \
    ${input_faa}.kmer.ix.8.sorted \
    ${input_faa}.kmer.ix.9.sorted \
    ${input_faa}.kmer.ix.10.sorted \
    ${input_faa}.kmer.ix.11.sorted \
    ${input_faa}.kmer.ix.12.sorted \
    ${input_faa}.kmer.ix.13.sorted \
    ${input_faa}.kmer.ix.14.sorted \
    ${input_faa}.kmer.ix.15.sorted \

cd ..
python database_size.py ${input_faa} ${index_dir}/${input_faa}.databasesize

echo "Finished OK"
