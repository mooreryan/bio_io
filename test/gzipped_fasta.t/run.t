Reads from normal files and fifo files.

  $ ../exes/parse_fasta.exe seqs.fa
  num seqs: 2, num bases: 12
  $ gunzip -c seqs.fa.gz | ../exes/parse_fasta.exe /dev/stdin
  num seqs: 2, num bases: 12
