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
EncodeKmers -i ${input_faa} -o ${index_dir}/${input_faa}.kmer.ix -t8

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
        > ${input_faa}.kmer.ix.sorted

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

BuildIndex2 -i ${input_faa}.kmer.ix.sorted -o ${input_faa}.kmer.ix1 -p ${input_faa}.kmer.ix2 -v -t8
cd ..
python database_size.py ${input_faa} ${index_dir}/${input_faa}.databasesize

echo "Finished OK"
