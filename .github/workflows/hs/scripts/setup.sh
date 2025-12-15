export DEBIAN_FRONTEND=noninteractive

#Install dependencies
apt-get update -y
apt-get install -y --no-install-recommends curl jq ca-certificates git
curl -sL https://aka.ms/InstallAzureCLIDeb | bash

#Install Actions Runner package
mkdir /home/actions
mkdir /home/actions/actions-runner
cd /home/actions/actions-runner
curl -o actions-runner-linux-x64-2.322.0.tar.gz -L https://github.com/actions/runner/releases/download/v2.322.0/actions-runner-linux-x64-2.322.0.tar.gz
tar xzf ./actions-runner-linux-x64-2.322.0.tar.gz

#Configure ephemeral runner, running as root
export RUNNER_ALLOW_RUNASROOT=1
./config.sh --url "${GH_SERVER_URL}/${GH_REPOSITORY_OWNER}/${REPO_NAME}" --ephemeral --token ${REG_TOKEN} --labels ${RUNNER_LABEL} --unattended

#Create service that starts runner, destroys VM when finished
cat > /etc/systemd/system/github-runner.service << EOF
[Unit]
Description=GitHub Runner

[Service]
Environment="RUNNER_ALLOW_RUNASROOT=1"
User=root
Type=oneshot
ExecStart=/bin/bash /home/actions/actions-runner/run.sh
ExecStart=/bin/bash /opt/devops/destroy_vm.sh

[Install]
WantedBy=multi-user.target
EOF

#Script to self-destruct VM, prevent env variable expansion
mkdir /opt/devops
HOSTNAME=$(hostname)
cat > /opt/devops/destroy_vm.sh << EOF
az login --identity > /dev/null 2>&1

az vm delete \
    --resource-group "${RESOURCE_GROUP}" \
    --name "${HOSTNAME}" \
    --yes --no-wait --force-deletion
EOF

systemctl start github-runner.service