open! Base
open Bio_io

let%test_unit "reverse complement" =
  let s = "ATUGCYRSWKMBDHVNatugcyrswkmbdhvn!@#$%" in
  let comp = "TAACGRYSWMKVHDBNtaacgryswmkvhdbn!@#$%" in
  let rev_comp = "%$#@!nbdhvkmwsyrgcaatNBDHVKMWSYRGCAAT" in
  assert (String.(comp = Utils.complement s)) ;
  assert (String.(rev_comp = Utils.rev_complement s))
