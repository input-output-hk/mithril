---
title: Mithril client CLI output breaking change
authors:
  - name: Mithril Team
tags: [mithril client, cli, breaking-change]
---

### Breaking change introduced in the output of Mithril client CLI

With the release of the new distribution [2408](https://github.com/input-output-hk/mithril/releases/tag/2408.0), we have introduced a breaking change to the **Mithril client CLI** version `0.7.0`: all logs are now written to `stderr` instead of `stdout`. This allows for cleaner command results that you can directly pipe into tools like `jq`.

Below, the `stdout` output of `mithril-client -vvv snapshot download latest --json` command before the update:

```
{"timestamp": "2024-02-27T10:41:55.576645+00:00", "step_num": 1, "total_steps": 5, "message": "Checking local disk info…"}
{"timestamp": "2024-02-27T10:41:55.576932+00:00", "step_num": 2, "total_steps": 5, "message": "Fetching the certificate and verifying the certificate chain…"}
{"timestamp": "2024-02-27T10:41:55.847199+00:00", "step_num": 3, "total_steps": 5, "message": "Downloading and unpacking the snapshot…"}
{"timestamp": "2024-02-27T10:41:56.023585+00:00", "bytes_downloaded": 390, "bytes_total": 345223208, "seconds_left": 75797.765, "seconds_elapsed": 0.085}
{"timestamp": "2024-02-27T10:41:56.356820+00:00", "bytes_downloaded": 9487846, "bytes_total": 345223208, "seconds_left": 32.674, "seconds_elapsed": 0.418}
{"timestamp": "2024-02-27T10:41:56.690001+00:00", "bytes_downloaded": 21682030, "bytes_total": 345223208, "seconds_left": 18.209, "seconds_elapsed": 0.752}
{"timestamp": "2024-02-27T10:41:57.023923+00:00", "bytes_downloaded": 33795639, "bytes_total": 345223208, "seconds_left": 14.218, "seconds_elapsed": 1.085}
{"timestamp": "2024-02-27T10:41:57.356999+00:00", "bytes_downloaded": 45934938, "bytes_total": 345223208, "seconds_left": 12.204, "seconds_elapsed": 1.419}
{"timestamp": "2024-02-27T10:41:57.690031+00:00", "bytes_downloaded": 58130472, "bytes_total": 345223208, "seconds_left": 10.894, "seconds_elapsed": 1.752}
{"timestamp": "2024-02-27T10:41:58.023964+00:00", "bytes_downloaded": 70235494, "bytes_total": 345223208, "seconds_left": 9.922, "seconds_elapsed": 2.086}
{"timestamp": "2024-02-27T10:41:58.357817+00:00", "bytes_downloaded": 82456663, "bytes_total": 345223208, "seconds_left": 9.134, "seconds_elapsed": 2.419}
{"timestamp": "2024-02-27T10:41:58.690945+00:00", "bytes_downloaded": 94618128, "bytes_total": 345223208, "seconds_left": 8.463, "seconds_elapsed": 2.753}
{"timestamp": "2024-02-27T10:41:59.024599+00:00", "bytes_downloaded": 106765259, "bytes_total": 345223208, "seconds_left": 7.868, "seconds_elapsed": 3.086}
{"timestamp": "2024-02-27T10:41:59.358139+00:00", "bytes_downloaded": 118941687, "bytes_total": 345223208, "seconds_left": 7.325, "seconds_elapsed": 3.420}
{"timestamp": "2024-02-27T10:41:59.691176+00:00", "bytes_downloaded": 131052374, "bytes_total": 345223208, "seconds_left": 6.824, "seconds_elapsed": 3.753}
{"timestamp": "2024-02-27T10:42:00.025189+00:00", "bytes_downloaded": 143190076, "bytes_total": 345223208, "seconds_left": 6.351, "seconds_elapsed": 4.087}
{"timestamp": "2024-02-27T10:42:00.358735+00:00", "bytes_downloaded": 155448192, "bytes_total": 345223208, "seconds_left": 5.896, "seconds_elapsed": 4.420}
{"timestamp": "2024-02-27T10:42:00.693529+00:00", "bytes_downloaded": 167494850, "bytes_total": 345223208, "seconds_left": 5.466, "seconds_elapsed": 4.755}
{"timestamp": "2024-02-27T10:42:01.026885+00:00", "bytes_downloaded": 179789000, "bytes_total": 345223208, "seconds_left": 5.043, "seconds_elapsed": 5.088}
{"timestamp": "2024-02-27T10:42:01.360483+00:00", "bytes_downloaded": 187536751, "bytes_total": 345223208, "seconds_left": 4.778, "seconds_elapsed": 5.422}
{"timestamp": "2024-02-27T10:42:01.693978+00:00", "bytes_downloaded": 199737576, "bytes_total": 345223208, "seconds_left": 4.389, "seconds_elapsed": 5.756}
{"timestamp": "2024-02-27T10:42:02.027113+00:00", "bytes_downloaded": 211879712, "bytes_total": 345223208, "seconds_left": 4.006, "seconds_elapsed": 6.089}
{"timestamp": "2024-02-27T10:42:02.360139+00:00", "bytes_downloaded": 224033698, "bytes_total": 345223208, "seconds_left": 3.626, "seconds_elapsed": 6.422}
{"timestamp": "2024-02-27T10:42:02.694407+00:00", "bytes_downloaded": 236212871, "bytes_total": 345223208, "seconds_left": 3.249, "seconds_elapsed": 6.756}
{"timestamp": "2024-02-27T10:42:03.027431+00:00", "bytes_downloaded": 248301091, "bytes_total": 345223208, "seconds_left": 2.878, "seconds_elapsed": 7.089}
{"timestamp": "2024-02-27T10:42:03.361001+00:00", "bytes_downloaded": 260428703, "bytes_total": 345223208, "seconds_left": 2.509, "seconds_elapsed": 7.423}
{"timestamp": "2024-02-27T10:42:03.694430+00:00", "bytes_downloaded": 272673595, "bytes_total": 345223208, "seconds_left": 2.139, "seconds_elapsed": 7.756}
{"timestamp": "2024-02-27T10:42:04.028557+00:00", "bytes_downloaded": 284878102, "bytes_total": 345223208, "seconds_left": 1.774, "seconds_elapsed": 8.090}
{"timestamp": "2024-02-27T10:42:04.361686+00:00", "bytes_downloaded": 296835374, "bytes_total": 345223208, "seconds_left": 1.418, "seconds_elapsed": 8.423}
{"timestamp": "2024-02-27T10:42:04.695532+00:00", "bytes_downloaded": 309005759, "bytes_total": 345223208, "seconds_left": 1.058, "seconds_elapsed": 8.757}
{"timestamp": "2024-02-27T10:42:05.028705+00:00", "bytes_downloaded": 321118015, "bytes_total": 345223208, "seconds_left": 0.702, "seconds_elapsed": 9.090}
{"timestamp": "2024-02-27T10:42:05.361956+00:00", "bytes_downloaded": 333264477, "bytes_total": 345223208, "seconds_left": 0.347, "seconds_elapsed": 9.424}
{"timestamp": "2024-02-27T10:42:05.711065+00:00", "step_num": 4, "total_steps": 5, "message": "Computing the snapshot message"}
{"timestamp": "2024-02-27T10:42:12.540752+00:00", "step_num": 5, "total_steps": 5, "message": "Verifying the snapshot signature…"}
{"timestamp": "2024-02-27T10:42:12.540913+00:00", "db_directory": "/mithril-client-0.5.17/db"}
```

Now the `stdout` output with **Mithril client CLI** version `0.7.0`:

```
{"timestamp": "2024-02-27T10:43:06.357962+00:00", "db_directory": "/mithril-client-0.7.0/db"}
```

In addition, the `--log-format-json` option that enable JSON output is now written to `stderr` as well.

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
