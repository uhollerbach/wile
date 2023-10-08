;;; (load "library/hash.scm")

(let ((test-cases '((#xcc #x24 #x31 #xc4 0)
		    (#xe0 #x4d #x9f #xcb 0)
		    (#x33 #x70 #x6a #x4e #x71 #x4d 0)
		    (#x35 #x52 #x30 #x4c #x67 #x37 0)
		    (#x37 #x6f #x45 #x34 #x38 #x36 0)
		    (#x42 #x52 #x34 #x32 #x71 #x66 0)
		    (#x46 #x6f #x75 #x42 #x53 #x72 0)
		    (#x47 #x6b #x6b #x7a #x46 #x44 0)
		    (#x48 #x56 #x47 #x5a #x71 #x39 0)
		    (#x49 #x77 #x50 #x53 #x64 #x54 0)
		    (#x60 #x6e #x72 #x59 #x33 #x47 0)
		    (#x71 #x7a #x73 #x30 #x55 #x44 0)
		    (#x73 #x58 #x62 #x73 #x73 #x72 0)
		    (#x75 #x68 #x34 #x74 #x53 #x49 0)
		    (#x12 #x3b #x5b #x23 #x20 0)
		    (#x2b #x21 #x3d #x79 #x47 0)
		    (#x2f #x06 #x3c #x37 #x71 0)
		    (#x36 #x38 #x6d #x2a #x20 0)
		    (#x40 #x24 #x1b #x38 #x42 0)
		    (#x4c #x39 #x59 #x56 #x11 0)
		    (#x59 #x08 #x26 #x60 #x62 0)
		    (#x60 #x4a #x63 #x6f #x11 0)
		    (#x65 #x53 #x4e #x2e #x31 0))))
  (write-string "test string-hash-32\n")
  (for-each (lambda (tc)
	      (let ((h (string-hash-32
			(apply char->string (map integer->char tc)))))
		(unless (zero? h)
		  (printf "%v\t%d\tFAIL\n" tc h))))
	    test-cases)
  (write-string "test ends\n"))

(let ((test-cases '((#x37 #x37 #x6b #x65 #x70 #x51 #x46 #x51 #x38 #x4b #x6c 0)
		    (#x07 #x1e #x62 #x37 #x2c #x02 #x40 #x42 #x14 #x69 0)
		    (#x0a #x4d #x3e #x21 #x16 #x68 #x02 #x0e #x0c #x6d 0)
		    (#x0a #x59 #x59 #x0e #x24 #x44 #x2a #x7b #x60 #x70 0)
		    (#x0f #x73 #x15 #x07 #x1b #x6f #x38 #x4b #x17 #x39 0)
		    (#x10 #x28 #x59 #x4f #x6d #x07 #x0f #x45 #x3f #x2e 0)
		    (#x16 #x43 #x29 #x25 #x71 #x1b #x5c #x65 #x7a #x03 0)
		    (#x1c #x7f #x21 #x5c #x2d #x09 #x1a #x03 #x7f #x69 0)
		    (#x21 #x30 #x49 #x43 #x3d #x56 #x6c #x6f #x61 #x59 0)
		    (#x26 #x04 #x5d #x12 #x7c #x7d #x66 #x61 #x0c #x26 0)
		    (#x3d #x2e #x20 #x68 #x78 #x22 #x69 #x58 #x3c #x3b 0)
		    (#x3d #x66 #x7f #x2f #x6e #x36 #x53 #x40 #x30 #x5d 0)
		    (#x42 #x21 #x21 #x7b #x16 #x0e #x2f #x65 #x67 #x25 0)
		    (#x46 #x2e #x24 #x74 #x64 #x6f #x3c #x01 #x07 #x24 0)
		    (#x51 #x76 #x58 #x74 #x4d #x3e #x40 #x46 #x70 #x25 0)
		    (#x54 #x14 #x18 #x68 #x68 #x4b #x14 #x3c #x0a #x72 0)
		    (#x58 #x59 #x54 #x4d #x5c #x4d #x45 #x46 #x0d #x7c 0)
		    (#x5f #x22 #x6b #x57 #x6b #x3d #x2d #x76 #x24 #x63 0)
		    (#x60 #x22 #x10 #x0e #x59 #x21 #x65 #x39 #x7a #x69 0)
		    (#x65 #x05 #x78 #x75 #x4f #x47 #x3a #x15 #x6a #x23 0)
		    (#x65 #x79 #x15 #x0c #x1d #x73 #x32 #x40 #x5a #x26 0)
		    (#x67 #x2c #x21 #x55 #x66 #x46 #x29 #x7b #x68 #x04 0)
		    (#x01 #x1c #x4f #x49 #x78 #x7e #x12 #x6e #x6d #x61 0)
		    (#x05 #x46 #x2b #x0d #x6f #x5f #x67 #x1d #x63 #x49 0)
		    (#x09 #x1c #x11 #x5b #x68 #x01 #x09 #x23 #x37 #x48 0)
		    (#x09 #x5c #x02 #x44 #x11 #x06 #x58 #x5d #x27 #x3f 0)
		    (#x13 #x67 #x10 #x1b #x08 #x30 #x2a #x55 #x71 #x2a 0)
		    (#x20 #x0e #x75 #x1d #x51 #x65 #x72 #x0a #x62 #x0b 0)
		    (#x21 #x41 #x46 #x17 #x02 #x63 #x03 #x5b #x5b #x76 0)
		    (#x22 #x42 #x59 #x2e #x18 #x0a #x14 #x6f #x57 #x72 0)
		    (#x2d #x64 #x37 #x3d #x74 #x48 #x18 #x3e #x38 #x4e 0)
		    (#x30 #x5e #x64 #x66 #x59 #x24 #x65 #x64 #x5f #x06 0)
		    (#x31 #x51 #x18 #x5b #x43 #x5f #x3a #x6b #x60 #x49 0)
		    (#x36 #x48 #x0e #x56 #x31 #x6f #x17 #x59 #x38 #x0d 0)
		    (#x3d #x03 #x4f #x76 #x44 #x69 #x7b #x5b #x3d #x04 0)
		    (#x4d #x1a #x06 #x77 #x15 #x33 #x08 #x4d #x01 #x21 0)
		    (#x56 #x1b #x0d #x31 #x28 #x39 #x04 #x09 #x35 #x63 0)
		    (#x56 #x35 #x71 #x38 #x31 #x4a #x13 #x14 #x74 #x53 0)
		    (#x5f #x01 #x62 #x60 #x20 #x4f #x4a #x62 #x5c #x42 0)
		    (#x65 #x68 #x4f #x66 #x68 #x21 #x3b #x07 #x06 #x02 0)
		    (#x69 #x59 #x67 #x0c #x2a #x65 #x07 #x0d #x46 #x0d 0)
		    (#x69 #x5c #x69 #x45 #x1a #x78 #x3f #x7b #x49 #x21 0)
		    (#x6c #x03 #x4c #x6c #x0f #x05 #x4a #x4e #x7c #x1c 0)
		    (#x6c #x38 #x33 #x4a #x46 #x30 #x40 #x15 #x60 #x63 0)
		    (#x6c #x55 #x0c #x32 #x30 #x01 #x28 #x2f #x2d #x25 0)
		    (#x6e #x09 #x5a #x3a #x1d #x2b #x63 #x77 #x16 #x44 0)
		    (#x6e #x70 #x20 #x3e #x33 #x19 #x4a #x0f #x07 #x77 0)
		    (#x7a #x47 #x54 #x16 #x6e #x75 #x42 #x78 #x2f #x48 0)
		    (#x7b #x3e #x68 #x34 #x5f #x71 #x43 #x2a #x13 #x12 0)
		    (#xd5 #x6b #xb9 #x53 #x42 #x87 #x08 #x36 0))))
  (write-string "test string-hash-64\n")
  (for-each (lambda (tc)
	      (let ((h (string-hash-64
			(apply char->string (map integer->char tc)))))
		(unless (zero? h)
		  (printf "%v\t%d\tFAIL\n" tc h))))
	    test-cases)
  (write-string "test ends\n"))

(let ((test-cases '("string1"
		    "sTrInG2"
		    "STRING3")))
  (write-string "test string-ci-hash-32\n")
  (for-each (lambda (str)
	      (let* ((slo (string-downcase str))
		     (h1 (string-ci-hash-32 str))
		     (h2 (string-ci-hash-32 slo))
		     (h3 (string-hash-32 slo)))
		(if (and (= h1 h2) (= h1 h3))
		    (printf "%s -> %d\n" str h1)
		    (printf "%s -> %d %d %d FAIL\n" str h1 h2 h3))))
	    test-cases)
  (write-string "test ends\n"))

(let ((test-cases '("string1"
		    "sTrInG2"
		    "STRING3")))
  (write-string "test string-ci-hash-64\n")
  (for-each (lambda (str)
	      (let* ((slo (string-downcase str))
		     (h1 (string-ci-hash-64 str))
		     (h2 (string-ci-hash-64 slo))
		     (h3 (string-hash-64 slo)))
		(if (and (= h1 h2) (= h1 h3))
		    (printf "%s -> %d\n" str h1)
		    (printf "%s -> %d %d %d FAIL\n" str h1 h2 h3))))
	    test-cases)
  (write-string "test ends\n"))