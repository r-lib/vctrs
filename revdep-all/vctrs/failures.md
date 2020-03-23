# arrow

<details>

* Version: 0.16.0.2
* Source code: https://github.com/cran/arrow
* URL: https://github.com/apache/arrow/, https://arrow.apache.org/docs/r
* BugReports: https://issues.apache.org/jira/projects/ARROW/issues
* Date/Publication: 2020-02-14 12:20:05 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"arrow")` for more info

</details>

## In both

*   checking whether package ‘arrow’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/arrow/new/arrow.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘arrow’ ...
** package ‘arrow’ successfully unpacked and MD5 sums checked
** using staged installation
Downloading apache-arrow
rm: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow: Directory not empty
Mon Mar 23 18:10:36 CET 2020: Auto-brewing apache-arrow in /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow...
Error: Another active Homebrew vendor-install-ruby process is already in progress.
Please wait for it to finish or terminate it to continue.
Error: Failed to install vendor Ruby.
cp: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/Cellar/*/*/lib/*.a: No such file or directory
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrew.a
PKG_CFLAGS=-I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW
PKG_LIBS=-L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/lib -L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib -lbrewparquet -lbrewarrow_dataset -lbrewarrow -lbrewthrift -lbrewlz4 -lbrewboost_system -lbrewboost_filesystem -lbrewboost_regex -lbrewsnappy
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/arrow/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c array.cpp -o array.o
In file included from array.cpp:18:
././arrow_types.h:198:10: fatal error: 'arrow/api.h' file not found
#include <arrow/api.h>
         ^~~~~~~~~~~~~
1 error generated.
make: *** [array.o] Error 1
ERROR: compilation failed for package ‘arrow’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/arrow/new/arrow.Rcheck/arrow’

```
### CRAN

```
* installing *source* package ‘arrow’ ...
** package ‘arrow’ successfully unpacked and MD5 sums checked
** using staged installation
Downloading apache-arrow
rm: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/Library/Homebrew/cmd/tap-unpin.rb: Invalid argument
rm: fts_read: No such file or directory
Mon Mar 23 18:10:32 CET 2020: Auto-brewing apache-arrow in /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow...
==> Tapping autobrew/core from https://github.com/autobrew/homebrew-core
Tapped 2 commands and 4640 formulae (4,888 files, 12.7MB).
boost
lz4
openssl
thrift
snappy
==> Downloading https://homebrew.bintray.com/bottles/boost-1.67.0_1.mojave.bottle.tar.gz
==> Pouring boost-1.67.0_1.mojave.bottle.tar.gz
==> Skipping post_install step for autobrew...
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/boost/1.67.0_1: 13,506 files, 456.4MB
==> Downloading https://homebrew.bintray.com/bottles/lz4-1.8.3.mojave.bottle.tar.gz
==> Pouring lz4-1.8.3.mojave.bottle.tar.gz
==> Skipping post_install step for autobrew...
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/lz4/1.8.3: 22 files, 512.7KB
==> Downloading https://homebrew.bintray.com/bottles/openssl-1.0.2p.mojave.bottle.tar.gz
==> Pouring openssl-1.0.2p.mojave.bottle.tar.gz
==> Skipping post_install step for autobrew...
==> Caveats
openssl is keg-only, which means it was not symlinked into /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow,
because Apple has deprecated use of OpenSSL in favor of its own TLS and crypto libraries.

If you need to have openssl first in your PATH run:
  echo 'export PATH="/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/opt/openssl/bin:$PATH"' >> ~/.zshrc

For compilers to find openssl you may need to set:
  export LDFLAGS="-L/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/opt/openssl/lib"
  export CPPFLAGS="-I/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/opt/openssl/include"

For pkg-config to find openssl you may need to set:
  export PKG_CONFIG_PATH="/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/opt/openssl/lib/pkgconfig"

==> Summary
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/openssl/1.0.2p: 1,793 files, 12MB
==> Downloading https://homebrew.bintray.com/bottles/thrift-0.11.0.mojave.bottle.tar.gz
==> Pouring thrift-0.11.0.mojave.bottle.tar.gz
==> Skipping post_install step for autobrew...
==> Caveats
To install Ruby binding:
  gem install thrift
==> Summary
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/thrift/0.11.0: 102 files, 7MB
==> Downloading https://homebrew.bintray.com/bottles/snappy-1.1.7_1.mojave.bottle.tar.gz
==> Pouring snappy-1.1.7_1.mojave.bottle.tar.gz
==> Skipping post_install step for autobrew...
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/snappy/1.1.7_1: 18 files, 115.8KB
==> Downloading https://autobrew.github.io/bottles/apache-arrow-0.16.0.el_capitan.bottle.tar.gz
==> Pouring apache-arrow-0.16.0.el_capitan.bottle.tar.gz
==> Skipping post_install step for autobrew...
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/apache-arrow/0.16.0: 278 files, 46.8MB
==> Caveats
==> openssl
openssl is keg-only, which means it was not symlinked into /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow,
because Apple has deprecated use of OpenSSL in favor of its own TLS and crypto libraries.

If you need to have openssl first in your PATH run:
  echo 'export PATH="/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/opt/openssl/bin:$PATH"' >> ~/.zshrc

For compilers to find openssl you may need to set:
  export LDFLAGS="-L/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/opt/openssl/lib"
  export CPPFLAGS="-I/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/opt/openssl/include"

For pkg-config to find openssl you may need to set:
  export PKG_CONFIG_PATH="/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/opt/openssl/lib/pkgconfig"

==> thrift
To install Ruby binding:
  gem install thrift
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewarrow.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewarrow_dataset.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewparquet.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_atomic-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_chrono-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_chrono.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_container-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_container.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_context-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_contract-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_contract.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_coroutine-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_coroutine.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_date_time-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_date_time.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_exception-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_exception.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_fiber-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_filesystem-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_filesystem.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_graph-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_graph.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_iostreams-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_iostreams.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_locale-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_log-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_log.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_log_setup-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_log_setup.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_c99-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_c99.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_c99f-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_c99f.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_c99l-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_c99l.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_tr1-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_tr1.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_tr1f-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_tr1f.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_tr1l-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_math_tr1l.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_prg_exec_monitor-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_prg_exec_monitor.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_program_options-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_program_options.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_random-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_random.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_regex-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_regex.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_serialization-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_serialization.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_signals-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_signals.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_stacktrace_addr2line-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_stacktrace_addr2line.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_stacktrace_basic-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_stacktrace_basic.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_stacktrace_noop-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_stacktrace_noop.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_system-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_system.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_test_exec_monitor-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_test_exec_monitor.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_thread-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_timer-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_timer.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_type_erasure-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_type_erasure.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_unit_test_framework-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_unit_test_framework.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_wave-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_wserialization-mt.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewboost_wserialization.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewlz4.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewcrypto.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewssl.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewsnappy.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewthrift.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewthriftz.a
PKG_CFLAGS=-I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW
PKG_LIBS=-L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/lib -L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib -lbrewparquet -lbrewarrow_dataset -lbrewarrow -lbrewthrift -lbrewlz4 -lbrewboost_system -lbrewboost_filesystem -lbrewboost_regex -lbrewsnappy
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/arrow/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c array.cpp -o array.o
In file included from array.cpp:18:
In file included from ././arrow_types.h:201:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/api.h:22:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/file_base.h:29:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/scanner.h:32:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:22:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from array.cpp:18:
In file included from ././arrow_types.h:201:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/api.h:22:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/file_base.h:29:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/scanner.h:32:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:22:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from array.cpp:18:
In file included from ././arrow_types.h:201:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/api.h:22:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/file_base.h:29:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/scanner.h:32:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:22:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from array.cpp:18:
In file included from ././arrow_types.h:201:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/api.h:22:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/file_base.h:29:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/scanner.h:32:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:22:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from array.cpp:18:
In file included from ././arrow_types.h:201:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/api.h:22:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/file_base.h:29:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/dataset/scanner.h:32:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:22:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [array.o] Error 1
ERROR: compilation failed for package ‘arrow’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/arrow/old/arrow.Rcheck/arrow’

```
# ipaddress

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/ipaddress
* URL: https://davidchall.github.io/ipaddress, https://github.com/davidchall/ipaddress
* BugReports: https://github.com/davidchall/ipaddress/issues
* Date/Publication: 2020-03-10 16:00:02 UTC
* Number of recursive dependencies: 27

Run `revdep_details(,"ipaddress")` for more info

</details>

## In both

*   checking whether package ‘ipaddress’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/ipaddress/new/ipaddress.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ipaddress’ ...
** package ‘ipaddress’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include" -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c IpAddressVector.cpp -o IpAddressVector.o
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [IpAddressVector.o] Error 1
ERROR: compilation failed for package ‘ipaddress’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/ipaddress/new/ipaddress.Rcheck/ipaddress’

```
### CRAN

```
* installing *source* package ‘ipaddress’ ...
** package ‘ipaddress’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include" -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c IpAddressVector.cpp -o IpAddressVector.o
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:7:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [IpAddressVector.o] Error 1
ERROR: compilation failed for package ‘ipaddress’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/ipaddress/old/ipaddress.Rcheck/ipaddress’

```
# sf

<details>

* Version: 0.8-1
* Source code: https://github.com/cran/sf
* URL: https://github.com/r-spatial/sf/, https://r-spatial.github.io/sf/
* BugReports: https://github.com/r-spatial/sf/issues/
* Date/Publication: 2020-01-28 11:20:07 UTC
* Number of recursive dependencies: 131

Run `revdep_details(,"sf")` for more info

</details>

## In both

*   checking whether package ‘sf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/sf/new/sf.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
** using staged installation
configure: CC: clang
configure: CXX: clang++ -std=gnu++11 -std=c++11
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.4.2
checking GDAL version >= 2.0.1... yes
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking how to run the C preprocessor... clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... yes
checking GDAL: /usr/local/Cellar/gdal/2.4.2_3/share/gdal/pcs.csv readable... yes
checking GDAL: checking whether PROJ is available for linking:... yes
checking GDAL: checking whether PROJ is available fur running:... yes
configure: pkg-config proj exists, will use it
configure: using proj.h.
configure: PROJ: 6.2.1
checking PROJ: checking whether PROJ and sqlite3 are available for linking:... yes
checking for geos-config... /usr/local/bin/geos-config
checking geos-config usability... yes
configure: GEOS: 3.8.0
checking GEOS version >= 3.4.0... yes
checking geos_c.h usability... yes
checking geos_c.h presence... yes
checking for geos_c.h... yes
checking geos: linking with -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c... yes
configure: Package CPP flags:  -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include
configure: Package LIBS: -L/usr/local/Cellar/proj/6.2.1/lib -lproj   -L/usr/local/Cellar/gdal/2.4.2_3/lib -lgdal -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c
configure: creating ./config.status
config.status: creating src/Makevars
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c bbox.cpp -o bbox.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal.cpp -o gdal.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal_geom.cpp -o gdal_geom.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal_read.cpp -o gdal_read.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal_utils.cpp -o gdal_utils.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal_write.cpp -o gdal_write.o
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [gdal_write.o] Error 1
ERROR: compilation failed for package ‘sf’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/sf/new/sf.Rcheck/sf’

```
### CRAN

```
* installing *source* package ‘sf’ ...
** package ‘sf’ successfully unpacked and MD5 sums checked
** using staged installation
configure: CC: clang
configure: CXX: clang++ -std=gnu++11 -std=c++11
checking for gdal-config... /usr/local/bin/gdal-config
checking gdal-config usability... yes
configure: GDAL: 2.4.2
checking GDAL version >= 2.0.1... yes
checking for gcc... clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ISO C89... none needed
checking how to run the C preprocessor... clang -E
checking for grep that handles long lines and -e... /usr/bin/grep
checking for egrep... /usr/bin/grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking gdal.h usability... yes
checking gdal.h presence... yes
checking for gdal.h... yes
checking GDAL: linking with --libs only... yes
checking GDAL: /usr/local/Cellar/gdal/2.4.2_3/share/gdal/pcs.csv readable... yes
checking GDAL: checking whether PROJ is available for linking:... yes
checking GDAL: checking whether PROJ is available fur running:... yes
configure: pkg-config proj exists, will use it
configure: using proj.h.
configure: PROJ: 6.2.1
checking PROJ: checking whether PROJ and sqlite3 are available for linking:... yes
checking for geos-config... /usr/local/bin/geos-config
checking geos-config usability... yes
configure: GEOS: 3.8.0
checking GEOS version >= 3.4.0... yes
checking geos_c.h usability... yes
checking geos_c.h presence... yes
checking for geos_c.h... yes
checking geos: linking with -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c... yes
configure: Package CPP flags:  -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include
configure: Package LIBS: -L/usr/local/Cellar/proj/6.2.1/lib -lproj   -L/usr/local/Cellar/gdal/2.4.2_3/lib -lgdal -L/usr/local/Cellar/geos/3.8.0/lib -lgeos_c
configure: creating ./config.status
config.status: creating src/Makevars
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c bbox.cpp -o bbox.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal.cpp -o gdal.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal_geom.cpp -o gdal_geom.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal_read.cpp -o gdal_read.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal_utils.cpp -o gdal_utils.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/usr/local/Cellar/proj/6.2.1/include -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.2_3/include -I/usr/local/Cellar/geos/3.8.0/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -g -O0 -c gdal_write.cpp -o gdal_write.o
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.2_3/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [gdal_write.o] Error 1
ERROR: compilation failed for package ‘sf’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/sf/old/sf.Rcheck/sf’

```
