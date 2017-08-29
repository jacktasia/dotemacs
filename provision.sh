#!/bin/bash

# setup keys
add-apt-repository ppa:kelleyk/emacs -y
apt-get update
apt-get install emacs25 -y

cp /vagrant/id_rsa /root/.ssh/id_rsa

su -l ubuntu
cp /vagrant/id_rsa /home/ubuntu/.ssh/id_rsa
chown ubuntu:ubuntu /home/ubuntu/.ssh/id_rsa
ssh -o StrictHostKeyChecking=no git@github.com
mkdir -p /home/ubuntu/code
chown -R ubuntu:ubuntu /home/ubuntu/code
cd code
git clone git@github.com:jacktasia/dotemacs.git
chown -R ubuntu:ubuntu /home/ubuntu/code

ln -s /home/ubuntu/code/dotemacs/dotemacs24.el /home/ubuntu/.emacs
