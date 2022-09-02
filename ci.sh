#!/bin/sh

cd "$(dirname "$0")"

EMACS="${EMACS:-$(which emacs)}"

echo "*** Emacs version ***"
echo "EMACS = ${EMACS}"
$EMACS --version
echo

curl -fsSLo /tmp/cask-master.zip https://github.com/cask/cask/archive/master.zip
sudo unzip -qq -d /opt /tmp/cask-master.zip
sudo ln -sf /opt/cask-master/bin/cask /usr/local/bin/cask
cask
cask exec $EMACS -batch -l dev/ert.el -l dev/examples-to-tests.el -l dev/undercover-init.el -l s.el -l dev/examples.el -f ert-run-tests-batch-and-exit
