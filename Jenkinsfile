// We use https://github.com/juspay/jenkins-nix-ci

pipeline {
    agent { label 'nixos' }
    stages {
        stage ('Matrix') {
            matrix {
                agent {
                    label "${NIX_SYSTEM}"
                }
                axes {
                    axis {
                        name 'NIX_SYSTEM'
                        values 'x86_64-linux', 'aarch64-linux'
                    }
                }
                stages {
                    stage ('Build') {
                        steps {
                            nixCI ()
                        }
                    }
                }
            }
        }
    }
}
