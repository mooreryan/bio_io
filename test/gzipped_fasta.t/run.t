Reads from normal files and fifo files.

  $ ../exes/parse_fasta.exe seqs.fa
  num seqs: 2, num bases: 12
  $ gunzip -c seqs.fa.gz | ../exes/parse_fasta.exe /dev/stdin
  num seqs: 2, num bases: 12

Stdin in directly works as well.

  $ ../exes/fasta_stdin.exe < seqs.fa
  0 -- s1 -- ACTG
  1 -- s2 -- actgactg
  $ gunzip -c seqs.fa.gz | ../exes/fasta_stdin.exe -
  0 -- s1 -- ACTG
  1 -- s2 -- actgactg
