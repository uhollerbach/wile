(load-library "bigint.scm")

(bigint-set-base-bits! 29)

(let ((tst (bigint-to-string (bigint-factorial 250) 16))
      (cmp "#x10edd87ac1487b9881852e89aefef4f72da603731f45d58ede4bdd3edcc0e0302fa38d78c609f84b844411fc3a878fa2e505ed26214fa681b9c5e92c3acb6d129c913d333103f1e952ad546610ea5dfe05d87936849b02b5fba573b87cede1347f1fee1576a8d6e0aa9d0323cf310c13c7bcb0034c90f10359d8d44f13a5d8d1e1699673b5ef99d9672ab1392fb8607c7dc31ee79b9c5c25a85d1583bd046010e2853504be5e94130522c660a5f7b0000000000000000000000000000000000000000000000000000000000000"))
  (if (string=? tst cmp)
      (write-string "factorial is good: " tst "\n")
      (write-string "factorial FAILS: " tst "\n")))

(let ((tst (bigint-to-string (bigint-catalan 500) 16))
      (cmp "#x34cb8c23b41e80c2edd8726444f40ac4abf83150b05a809b9a784dbc0d5c76adb8aae56ab8bf8cd63865ad5b78d591a6909868fbb26bc4aba3d0b6a477acb2941108a42ad7c29a874c34f14ad667afa8d92af7d468c9540d05e22857bea1c534d31a86cb1524792b0396e66481c7b2fa7f798a0633349a1179c5940"))
  (if (string=? tst cmp)
      (write-string "catalan is good: " tst "\n")
      (write-string "catalan FAILS: " tst "\n")))

(let ((tst (bigint-to-string (list-ref (bigint-binomial-coeffs 1000) 500) 16))
      (cmp "#x67525941df7fb1fd7b7897de3af19912e48cb882e9211db0854d70210625f441fa6672f9db8ede9f3c5efc440379f60af8fa4574942ce7e3e797756fde34f97bc555e947d83fd86ac8239c377598eac571011702b112017d7c8398f3b81296f06122e9cb706061212e0650e2b1fbd1443b78db1e2235f184334faaa40"))
  (if (string=? tst cmp)
      (write-string "binomial is good: " tst "\n")
      (write-string "binomial FAILS: " tst "\n")))

(let ((tst (bigint-to-string (list-ref (bigint-stirling1-coeffs 400) 200) 16))
      (cmp "#x3efd90c72dfc1462919eb66f9c8a8b0a1bdeabeb0b81b62791692081b9e1150f8ebe0823624cb67130213b8f8549a44ba36a69aa966edf76a34c7b41f7b0f71794ff8e885e34981110dbdcd78a4e4b54fc9d20df08f392a928852f051934d0f40259bc1af3d2d0782620093d7eaf24d4196a4c5cfa9f6853334df581cc43cc02523ab22519496c2556a1a4f1833e7f2c4dd681c9d539d7a9345cbc15ccca8ad33b71aeda730730898ba9846ea6e55bd0c042c64b3318fbf02660621941cec7442d1d4493f0a5a9b48cb0d9270206883556fea9f872d54f3e70a6d38f855d0d3f6006f137bdfab7bfc71cb58951"))
  (if (string=? tst cmp)
      (write-string "stirling1 is good: " tst "\n")
      (write-string "stirling1 FAILS: " tst "\n")))

(let ((tst (bigint-to-string (list-ref (bigint-stirling2-coeffs 400) 200) 16))
      (cmp "#x1c3fbabda19e663fa4ec32104e27dd85add4a5158329cfd7fc45d28344ba58901548faecb4740610860440ed8d416b32cfb26cf96624feafcc73035b7f55805b608d79ed8ef447b6eaff1b9101cdd5893ef1aa6caf65aa6a558b0423932b58f8c9273baadecd9d6880214d693523eb6a6690dd225e8121aef9d0997cb581f49c3d37050e98967760575df7618250df28367c5a31fe96afe53cf583f4c55fe1969cee8eaeb54f782b9a4901e851844f54ed7a27fab9fc6dcb5b95f986c4056b17ca0e716a41bccb17a47b659fd581467259338d30784781075db668ca4"))
  (if (string=? tst cmp)
      (write-string "stirling2 is good: " tst "\n")
      (write-string "stirling2 FAILS: " tst "\n")))

;;; (bigint-jacobi-symbol a n)
