# arrow

<details>

* Version: 0.17.0
* Source code: https://github.com/cran/arrow
* URL: https://github.com/apache/arrow/, https://arrow.apache.org/docs/r
* BugReports: https://issues.apache.org/jira/projects/ARROW/issues
* Date/Publication: 2020-04-21 18:10:02 UTC
* Number of recursive dependencies: 58

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
*** Downloading apache-arrow
Sat Apr 25 12:16:34 CEST 2020: Auto-brewing apache-arrow in /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow...
Error: No available formula with the name "apache-arrow" 
==> Searching for a previously deleted formula (in the last month)...
Error: No previously deleted formula found.
Error: No similarly named formulae found.
==> Searching taps on GitHub...
==> Searching for similarly named formulae...
==> Searching taps...
These formulae were found in taps:
homebrew/linuxbrew-core/apache-arrow
homebrew/linuxbrew-core/apache-arrow-glib
To install one of them, run (for example):
  brew install homebrew/linuxbrew-core/apache-arrow
cp: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/Cellar/*/*/lib/*.a: No such file or directory
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrew.a
PKG_CFLAGS=-I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW
PKG_LIBS=-L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/lib -L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib -lbrewparquet -lbrewarrow_dataset -lbrewarrow -lbrewthrift -lbrewlz4 -lbrewsnappy
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/arrow/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c array.cpp -o array.o
In file included from array.cpp:18:
././arrow_types.h:199:10: fatal error: 'arrow/api.h' file not found
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
*** Downloading apache-arrow
rm: fts_read: No such file or directory
Sat Apr 25 12:16:33 CEST 2020: Auto-brewing apache-arrow in /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow...
==> Tapping autobrew/core from https://github.com/autobrew/homebrew-core
Cloning into '/private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Library/Taps/autobrew/homebrew-core'...
Updating files:  72% (3521/4863)Updating files:  73% (3550/4863)Updating files:  74% (3599/4863)Updating files:  75% (3648/4863)Updating files:  76% (3696/4863)Updating files:  77% (3745/4863)Updating files:  78% (3794/4863)Updating files:  79% (3842/4863)Updating files:  80% (3891/4863)Updating files:  81% (3940/4863)Updating files:  82% (3988/4863)Updating files:  83% (4037/4863)Updating files:  84% (4085/4863)Updating files:  85% (4134/4863)Updating files:  86% (4183/4863)Updating files:  87% (4231/4863)Updating files:  88% (4280/4863)Updating files:  89% (4329/4863)Updating files:  90% (4377/4863)Updating files:  91% (4426/4863)Updating files:  92% (4474/4863)Updating files:  93% (4523/4863)Updating files:  94% (4572/4863)Updating files:  95% (4620/4863)Updating files:  96% (4669/4863)Updating files:  97% (4718/4863)Updating files:  98% (4766/4863)Updating files:  99% (4815/4863)Updating files: 100% (4863/4863)Updating files: 100% (4863/4863), done.
Tapped 2 commands and 4640 formulae (4,889 files, 12.7MB).
==> Installing dependencies for apache-arrow: lz4, openssl, thrift, snappy
==> Installing apache-arrow dependency: lz4
==> Downloading https://homebrew.bintray.com/bottles/lz4-1.8.3.mojave.bottle.tar.gz
Already downloaded: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/downloads/b4158ef68d619dbf78935df6a42a70b8339a65bc8876cbb4446355ccd40fa5de--lz4-1.8.3.mojave.bottle.tar.gz
==> Pouring lz4-1.8.3.mojave.bottle.tar.gz
==> Skipping post_install step for autobrew...
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/lz4/1.8.3: 22 files, 512.7KB
==> Installing apache-arrow dependency: openssl
==> Downloading https://homebrew.bintray.com/bottles/openssl-1.0.2p.mojave.bottle.tar.gz
Already downloaded: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/downloads/fbb493745981c8b26c0fab115c76c2a70142bfde9e776c450277e9dfbbba0bb2--openssl-1.0.2p.mojave.bottle.tar.gz
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
==> Installing apache-arrow dependency: thrift
==> Downloading https://homebrew.bintray.com/bottles/thrift-0.11.0.mojave.bottle.tar.gz
Already downloaded: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/downloads/7e05ea11a9f7f924dd7f8f36252ec73a24958b7f214f71e3752a355e75e589bd--thrift-0.11.0.mojave.bottle.tar.gz
==> Pouring thrift-0.11.0.mojave.bottle.tar.gz
==> Skipping post_install step for autobrew...
==> Caveats
To install Ruby binding:
  gem install thrift
==> Summary
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/thrift/0.11.0: 102 files, 7MB
==> Installing apache-arrow dependency: snappy
==> Downloading https://homebrew.bintray.com/bottles/snappy-1.1.7_1.mojave.bottle.tar.gz
Already downloaded: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/downloads/1f09938804055499d1dd951b13b26d80c56eae359aa051284bf4f51d109a9f73--snappy-1.1.7_1.mojave.bottle.tar.gz
==> Pouring snappy-1.1.7_1.mojave.bottle.tar.gz
==> Skipping post_install step for autobrew...
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/snappy/1.1.7_1: 18 files, 115.8KB
==> Installing apache-arrow
==> Downloading https://autobrew.github.io/bottles/apache-arrow-0.17.0.el_capitan.bottle.tar.gz
Already downloaded: /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/downloads/7dcf2302ba174a5efb32eaa5b8fe0ae874f4a4671f575e126c79a524830054ae--apache-arrow-0.17.0.el_capitan.bottle.tar.gz
==> Pouring apache-arrow-0.17.0.el_capitan.bottle.tar.gz
==> Skipping post_install step for autobrew...
🍺  /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/build-apache-arrow/Cellar/apache-arrow/0.17.0: 294 files, 49.7MB
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
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewlz4.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewcrypto.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewssl.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewsnappy.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewthrift.a
created /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib/libbrewthriftz.a
PKG_CFLAGS=-I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW
PKG_LIBS=-L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/lib -L/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/lib -lbrewparquet -lbrewarrow_dataset -lbrewarrow -lbrewthrift -lbrewlz4 -lbrewsnappy
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/arrow/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c array.cpp -o array.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/arrow/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c array_from_vector.cpp -o array_from_vector.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include -DARROW_R_WITH_ARROW -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/arrow/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c array_to_vector.cpp -o array_to_vector.o
In file included from array_to_vector.cpp:21:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/parallel.h:24:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:21:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from array_to_vector.cpp:21:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/parallel.h:24:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:21:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from array_to_vector.cpp:21:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/parallel.h:24:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:21:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from array_to_vector.cpp:21:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/parallel.h:24:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:21:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from array_to_vector.cpp:21:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/parallel.h:24:
In file included from /var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T//build-apache-arrow/opt/apache-arrow/include/arrow/util/thread_pool.h:21:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [array_to_vector.o] Error 1
ERROR: compilation failed for package ‘arrow’
* removing ‘/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/checks.noindex/arrow/old/arrow.Rcheck/arrow’

```
# ipaddress

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/ipaddress
* URL: https://davidchall.github.io/ipaddress, https://github.com/davidchall/ipaddress
* BugReports: https://github.com/davidchall/ipaddress/issues
* Date/Publication: 2020-03-25 17:30:02 UTC
* Number of recursive dependencies: 41

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
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include" -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -DASIO_STANDALONE -DASIO_NO_DEPRECATED -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c IpAddressVector.cpp -o IpAddressVector.o
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
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
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include" -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -DASIO_STANDALONE -DASIO_NO_DEPRECATED -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c IpAddressVector.cpp -o IpAddressVector.o
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/ip/address_v4.hpp:18:
In file included from /Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/ipaddress/AsioHeaders/include/asio/detail/config.hpp:1012:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from IpAddressVector.cpp:1:
In file included from ./IpAddressVector.h:5:
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

* Version: 0.9-2
* Source code: https://github.com/cran/sf
* URL: https://r-spatial.github.io/sf/, https://github.com/r-spatial/sf/
* BugReports: https://github.com/r-spatial/sf/issues/
* Date/Publication: 2020-04-14 17:10:05 UTC
* Number of recursive dependencies: 145

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
configure: GDAL: 2.4.4
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
checking GDAL: /usr/local/Cellar/gdal/2.4.4_4/share/gdal/pcs.csv readable... yes
checking GDAL: checking whether PROJ is available for linking:... yes
checking GDAL: checking whether PROJ is available fur running:... yes
configure: GDAL: 2.4.4
checking proj.h usability... yes
checking proj.h presence... yes
checking for proj.h... yes
checking PROJ: checking whether PROJ and sqlite3 are available for linking:... yes
checking for geos-config... /usr/local/bin/geos-config
checking geos-config usability... yes
configure: GEOS: 3.8.1
checking GEOS version >= 3.4.0... yes
checking geos_c.h usability... yes
checking geos_c.h presence... yes
checking for geos_c.h... yes
checking geos: linking with -L/usr/local/Cellar/geos/3.8.1_1/lib -lgeos_c... yes
configure: Package CPP flags:   -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include
configure: Package LIBS:  -L/usr/local/Cellar/gdal/2.4.4_4/lib -lgdal -lproj  -L/usr/local/Cellar/gdal/2.4.4_4/lib -lgdal -L/usr/local/Cellar/geos/3.8.1_1/lib -lgeos_c
configure: creating ./config.status
config.status: creating src/Makevars
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c bbox.cpp -o bbox.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal.cpp -o gdal.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal_geom.cpp -o gdal_geom.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal_read.cpp -o gdal_read.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal_utils.cpp -o gdal_utils.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal_write.cpp -o gdal_write.o
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
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
configure: GDAL: 2.4.4
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
checking GDAL: /usr/local/Cellar/gdal/2.4.4_4/share/gdal/pcs.csv readable... yes
checking GDAL: checking whether PROJ is available for linking:... yes
checking GDAL: checking whether PROJ is available fur running:... yes
configure: GDAL: 2.4.4
checking proj.h usability... yes
checking proj.h presence... yes
checking for proj.h... yes
checking PROJ: checking whether PROJ and sqlite3 are available for linking:... yes
checking for geos-config... /usr/local/bin/geos-config
checking geos-config usability... yes
configure: GEOS: 3.8.1
checking GEOS version >= 3.4.0... yes
checking geos_c.h usability... yes
checking geos_c.h presence... yes
checking for geos_c.h... yes
checking geos: linking with -L/usr/local/Cellar/geos/3.8.1_1/lib -lgeos_c... yes
configure: Package CPP flags:   -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include
configure: Package LIBS:  -L/usr/local/Cellar/gdal/2.4.4_4/lib -lgdal -lproj  -L/usr/local/Cellar/gdal/2.4.4_4/lib -lgdal -L/usr/local/Cellar/geos/3.8.1_1/lib -lgeos_c
configure: creating ./config.status
config.status: creating src/Makevars
** libs
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c bbox.cpp -o bbox.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal.cpp -o gdal.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal_geom.cpp -o gdal_geom.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal_read.cpp -o gdal_read.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal_utils.cpp -o gdal_utils.o
clang++ -std=gnu++11 -std=c++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DHAVE_PROJ_H -I/usr/local/Cellar/gdal/2.4.4_4/include -I/usr/local/Cellar/geos/3.8.1_1/include -I"/Users/lionel/Dropbox/Projects/R/hadley/vctrs/revdep-all/vctrs/library.noindex/sf/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -arch x86_64 -ftemplate-depth-256 -Wall -pedantic -c gdal_write.cpp -o gdal_write.o
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: unknown type name 'uuid_t'
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from gdal_write.cpp:5:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogrsf_frmts.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_feature.h:35:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/ogr_featurestyle.h:33:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_conv.h:36:
In file included from /usr/local/Cellar/gdal/2.4.4_4/include/cpl_vsi.h:62:
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
