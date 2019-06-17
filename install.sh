#!/usr/bin/env bash

cat <<EOF > hsp
#!/usr/bin/env bash
HSP_DIR=`pwd`
HSP_PWD=\`pwd\`

# The default HspCustom directory is $HOME.
# Uncomment and edit the following line to choose another directory.
# export HSP_CUSTOM=path/to/otherdir

# The default macro directory is $HOME.
# Uncomment and edit these lines and choose new directories if desired.
# HSP_MACRO_DIR=path/to/otherdir
# HSP_GROUP_MACRO_DIR=path/to/otherdir
# export HSP_MACRO_DIR HSP_GROUP_MACRO_DIR

export HSP_PWD HSP_DIR
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