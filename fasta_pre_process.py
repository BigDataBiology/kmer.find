from sys import argv

input_fname = argv[1]
output_fname = argv[2]

def fasta_pre_process(infile, outfile):
    with open(infile, 'rt') as fasta_file:
        with open(outfile, 'wt') as out_file:
            seq = ''
            for line in fasta_file:
                if line[0] == '>' and seq == '':
                    #process the first line of the input file
                    header = line.split()[0]
                elif line[0] != '>':
                    #join the line with sequence
                    seq = seq + line.strip()
                elif (line[0] == '>' and seq != '') or line == '':
                    #in subsequent lines starting with '>', write the previous header and sequence to the output file.
                    #Then re_initialize the header and seq variables for the next record.
                    out_file.write(header + "\n" +  seq + "\n")
                    seq = ''
                    header = line.split()[0]
            out_file.write(header + "\n" +  seq + "\n")

fasta_pre_process(input_fname, output_fname)
