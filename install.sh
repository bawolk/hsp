#!/usr/bin/env bash

cat <<EOF > hsp
#!/usr/bin/env bash
HSP_DIR=`pwd`
HSP_PWD=\`pwd\`
HSP_CUSTOM=\$HSP_DIR
HSP_MACRO_DIR=\$HOME
HSP_GROUP_MACRO_DIR=\$HOME
export HSP_PWD HSP_DIR HSP_CUSTOM HSP_MACRO_DIR HSP_GROUP_MACRO_DIR
(cd \$HSP_DIR; stack exec -- hsp "\$@")
EOF
chmod +x hsp
if [ -d $HOME/.local/bin ]
then
    cp hsp $HOME/.local/bin
    echo "hsp script has been copied to $HOME/.local/bin"
else
    echo "$HOME/.local/bin does not exist."
    echo "Copy `pwd`/hsp to a directory in your PATH."
fi