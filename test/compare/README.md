## Comparisons

This directory is for comparing other WebSocket clients to Mint.

### Gun

After the merging of
[#17](https://github.com/NFIBrokerage/mint_web_socket/pull/17),
Mint.WebSocket is pretty comparable performance and conformance-wise to
[`ninenines/gun`](https://github.com/ninenines/gun).

Here are the timings for the section 9 (limits/performance) cases of the
Autobahn|Testsuite:

| Case no. | Mint.WebSocket (ms) | Gun (ms) |
|----------|---------------------|----------|
| 9.1.1    | 2                   | 2        |
| 9.1.2    | 6                   | 6        |
| 9.1.3    | 23                  | 21       |
| 9.1.4    | 126                 | 83       |
| 9.1.5    | 228                 | 198      |
| 9.1.6    | 731                 | 378      |
| 9.2.1    | 3                   | 2        |
| 9.2.2    | 16                  | 5        |
| 9.2.3    | 17                  | 15       |
| 9.2.4    | 79                  | 58       |
| 9.2.5    | 224                 | 133      |
| 9.2.6    | 638                 | 272      |
| 9.3.1    | 158                 | 216      |
| 9.3.2    | 104                 | 116      |
| 9.3.3    | 87                  | 101      |
| 9.3.4    | 85                  | 79       |
| 9.3.5    | 116                 | 74       |
| 9.3.6    | 98                  | 89       |
| 9.3.7    | 129                 | 70       |
| 9.3.8    | 82                  | 74       |
| 9.3.9    | 98                  | 69       |
| 9.4.1    | 192                 | 180      |
| 9.4.2    | 101                 | 84       |
| 9.4.3    | 80                  | 62       |
| 9.4.4    | 100                 | 58       |
| 9.4.5    | 63                  | 63       |
| 9.4.6    | 62                  | 50       |
| 9.4.7    | 62                  | 49       |
| 9.4.8    | 69                  | 49       |
| 9.4.9    | 74                  | 47       |
| 9.5.1    | 226                 | 279      |
| 9.5.2    | 130                 | 145      |
| 9.5.3    | 71                  | 65       |
| 9.5.4    | 64                  | 44       |
| 9.5.5    | 61                  | 26       |
| 9.5.6    | 49                  | 21       |
| 9.6.1    | 211                 | 199      |
| 9.6.2    | 123                 | 123      |
| 9.6.3    | 99                  | 74       |
| 9.6.4    | 45                  | 60       |
| 9.6.5    | 30                  | 28       |
| 9.6.6    | 31                  | 19       |
| 9.7.1    | 131                 | 210      |
| 9.7.2    | 569                 | 138      |
| 9.7.3    | 113                 | 76       |
| 9.7.4    | 113                 | 135      |
| 9.7.5    | 147                 | 102      |
| 9.7.6    | 571                 | 571      |
| 9.8.1    | 105                 | 107      |
| 9.8.2    | 57                  | 102      |
| 9.8.3    | 95                  | 80       |
| 9.8.4    | 79                  | 115      |
| 9.8.5    | 299                 | 86       |
| 9.8.6    | 424                 | 160      |

The gun implementation is included in the `./gun` subdirectory.
