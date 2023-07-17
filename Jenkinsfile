// We use https://github.com/juspay/jenkins-nix-ci

pipeline {
    agent { label 'nixos' }
    stages {
        stage ('Build (native)') {
            steps {
                nixCI ()
            }
        }
        stage ('Build (arm)') {
            steps {
                nixCI system: 'aarch64-linux'
            }
        }
    }
}
