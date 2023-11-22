// We use https://github.com/juspay/jenkins-nix-ci

pipeline {
    agent { label 'nixos' }
    stages {
        stage ('Build') {
            steps {
                nixCI ()
            }
        }
    }
}
