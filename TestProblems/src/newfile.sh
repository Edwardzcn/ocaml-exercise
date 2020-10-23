#! /bin/bash
# input new user name


echo "Please input your name to create new .ml file with your name as suffix:"
read -p "-> " name_suffix

echo "===== now create _${name_suffix}.ml in subdirectory ====="

for subdir in `ls $PWD`
do
    if [ -d $subdir ]
    then
        cd $subdir
        if [ -e "${subdir}_${name_suffix}.ml" ]
        then
            echo "| Fail to create file in $subdir. The file exists"
            cd ..
        else
            echo "| Create file ${subdir}_${name_suffix}.ml"
            touch "${subdir}_${name_suffix}.ml"
            cd ..
        fi
    else
        echo "| This is not a directory"
        continue
    fi
done
