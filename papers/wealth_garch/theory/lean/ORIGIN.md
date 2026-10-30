# Origin of the Lean files

Every file of `theory/lean/` is a copy of the file of the same name under
`lean/mathlib/` of the `wealth_garch` bundle at commit `740823e4` (28 October 2026),
the last commit before work for another paper changed any of them. TH-1 of
`verify_theory.py` holds each file to the sha256 below. A file is changed only by
editing it here and updating its pin in the same commit, with the reason in `TODO.md`.

`ChoulliTaksarZhou.lean` is not a copy. It was written for this paper on 30 October
2026, with the record `theory/lit/choulli_taksar_zhou_2004/`, and is pinned the same way.
`TimeVarying.lean` is not a copy either. It was written for row M15 on 30 October 2026.

| File | sha256 |
|---|---|
| `AlvarezLeBihanLippi.lean` | `95d90fde159261f2b3ecf764cbd094e6a82930a99bbda5416099c51a409416c7` |
| `BarberisHuangSantos.lean` | `b9f6777a68d948a72b4ebee055b61779e73bab83fc7dc4ee0083796f07d4174a` |
| `BCVW.lean` | `5b9b55b0bf24f4e9798a2f8eb0ca92f3fefd5403e9cf6bd9ea7930e74649c42b` |
| `BehnEtAl.lean` | `9d8c2e99e6d5ed64c372fdd7fba98fe95d3fdd12b38e85fcb55560c1ac38625f` |
| `BergerEtAl.lean` | `a5586b41f25a4b93cb1cecae533571cd4743d19d4809b1a03af147533694c331` |
| `BlackPerold.lean` | `e955c31e3af2384c109872e11fabdb65b48a5212702306778b1c5bebd9e3e714` |
| `BoltonChenWang.lean` | `60254c8d9cff9ead2a2aacbb8503d5d3f6c727c4dd800b808c249c8d1b9fa312` |
| `BonaparteCooper.lean` | `364ce20d77354e7aa6b58c686546d9496d10d730aec4667a99f0315c151d192a` |
| `Bounds.lean` | `b393b4d4bd53cc5838dcda86478e53eb838710911ab73b308cd0282fb60d171a` |
| `CapGeometry.lean` | `72b241911572466fc65ec10002e4199ec2ccc17253e07075981cd42b8532d533` |
| `ChoulliTaksarZhou.lean` | `d664e3f5ba1ed883f59d6114bed24526d02a4909d2597fd7ff1105dcbd42487d` |
| `Comparison.lean` | `037d9b6a173375de6861ec8a6ef945c6e8d48317424623379907048a4a4e6344` |
| `Constantinides.lean` | `8a0170b14b3197072a389c9a4934ddeb23874c3acad61e7e9813f7a11fffb96a` |
| `Coordinate.lean` | `5c69f1b4bfa216f55c3fe1997ca3df9b85d19742cf83d5556453d370eca65847` |
| `DaiHuangKeppo.lean` | `955b510fe4df8d2deeb8b6b8f6c6ae587a431bd165d7ff072b3d9defc8865000` |
| `DecampsEtAl.lean` | `bc0484046090e82224c4c00f7720dfd4b609f6f702395c88fe03bc069d71f766` |
| `EgamiOryu.lean` | `b2e877e32b0564fbe12a530ab0e2a5b69a16626a3c78103022c0f0d834b3f1d1` |
| `EngleSiriwardane.lean` | `f80a488edd4b82c7bafdcd1f43dd352ab673bcc8f84ed4a1e40478da1a2931fe` |
| `Envelope.lean` | `1c55abd7dde76c00be3bb9436df48cc0348342913c5f7684f344b39f5d067467` |
| `FedericoEtAl.lean` | `3cf75a9320fae5b08b289e7bf69dc08475134541a972a5a62ae38237ff46eca6` |
| `GloverEtAl.lean` | `50898e9163a282964a2c6e511ba2d8327872630050d433fb6fa33d75f5e1b6e3` |
| `Identification.lean` | `2578829ed11faf51f36103e6d5587375d14ef42d264f4470e3845d1ee139ca71` |
| `ImpulseCount.lean` | `ab1aaa52ca9f4ab3ec9468d013e7c77e4b4bb8fab18b11fe1084e7ae486de66e` |
| `LambertiniUysal.lean` | `03e872dcdff28cc69e768ee0c96772e83784cce2dadb441b2dae7d3fb594a569` |
| `LangMenno.lean` | `11290e8fd36027df6da8bdd96fcccf4944d664476d36cafe9bc3b7f723bef250` |
| `LiYuZhang.lean` | `1038c7814fc4f4266e6ff49178af88beca4d43dc2cc5160a8fd171f4f986e1f7` |
| `LokkaZervos.lean` | `9a4d86bc2b74321167c1ce66180c7f72b4ec937c409c42d501a98bea8e83a7ed` |
| `MilneWhalley.lean` | `0f7b01c10798cf4492cee75e10871bf91ea6abf79db7714cba34506c059594dd` |
| `PeuraKeppo.lean` | `9807e3de496fcda436496f7c32fff3811fb81c3227c383f94997201db0b87bc9` |
| `QVI_Part1.lean` | `5d2d9424c0b79a42abcb67ad8db76f75f4fbeae5449eadd240468b10863ff475` |
| `RateBased.lean` | `30c6b9f2a05619a083cbffb3f9f97ff8534a4bf42fcdf7a4e5db208ec0fc892f` |
| `SecondOrder.lean` | `054c0c198727680fbb715b8af938a32a143cb3936d04681184fd2708576c85ab` |
| `SmoothFit.lean` | `e5b8e1c9d3b32c52cfc1b91eabf05ee94e5d73940ccedc369bf735488ff399fc` |
| `Targeting.lean` | `b03443644a619d279612f0d630ccd2d89a1201c6ad0672c23170a5cb340fbf95` |
| `TimeVarying.lean` | `a7ed38f4f793a81ef1713b779f4d09d654e639757beac2914288cf88b966fa96` |
