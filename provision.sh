#!/bin/bash

# setup keys
ssh -o StrictHostKeyChecking=no git@github.com
cp /vagrant/id_rsa /root/.ssh/id_rsa

add-apt-repository ppa:kelleyk/emacs -y
apt-get update
apt-get install emacs25 -y


mkdir -p /home/ubuntu/code
cd code
git clone git@github.com:jacktasia/dotemacs.git

ln -s /home/ubuntu/code/dotemacs/dotemacs24.el /home/ubuntu/.emacs
