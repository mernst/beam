#!/bin/bash
#
#    Licensed to the Apache Software Foundation (ASF) under one or more
#    contributor license agreements.  See the NOTICE file distributed with
#    this work for additional information regarding copyright ownership.
#    The ASF licenses this file to You under the Apache License, Version 2.0
#    (the "License"); you may not use this file except in compliance with
#    the License.  You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.
#

# This script will create a Release Candidate, includes:
# 1. Build and stage java artifacts
# 2. Stage source release on dist.apache.org
# 3. Stage python binaries
# 4. Create a PR to update beam-site

set -e

set -e

function usage() {
  echo 'Usage: build_release_candidate.sh --version <version> --rc-num <num> --signing-key <gpg id> [--no-source] [--no-java] [--no-python] [--no-docs] [--doc-pr-user <github id>]'
}

while [[ $# -gt 0 ]] ; do
  arg="$1"

  case $arg in
      --debug)
        set -x
        shift
      ;;

      --signing-key)
        SIGNING_KEY="$1"
        shift
        shift
      ;;

      --no-source)
        NO_SOURCE=true
        shift
      ;;

      --no-java)
        NO_JAVA=true
        shift
      ;;

      --no-python)
        NO_PYTHON=true
        shift
      ;;

      --no-docs)
        NO_DOCS=true
        shift
      ;;

      --doc-pr-user)
        DOC_PR_USER="$1"
        shift
        shift
      ;;

      --rc-num)
        RC_NUM="$1"
        shift
        shift
      ;;

      --version)
        RELEASE_VERSION="$1"
        shift
        shift
      ;;

      *)
        echo "Unknown argument: $1"
        usage
        exit 1
      ;;
   esac
done


LOCAL_PYTHON_VIRTUALENV=${LOCAL_PYTHON_STAGING_DIR}/venv
LOCAL_WEBSITE_UPDATE_DIR=website_update_dir
LOCAL_PYTHON_DOC=python_doc
LOCAL_JAVA_DOC=java_doc
LOCAL_WEBSITE_REPO=beam_website_repo

GIT_REPO_URL=git@github.com:apache/beam.git
ROOT_SVN_URL=https://dist.apache.org/repos/dist/dev/beam
GIT_BEAM_ARCHIVE=https://github.com/apache/beam/archive
GIT_BEAM_WEBSITE=https://github.com/apache/beam-site.git

PYTHON_ARTIFACTS_DIR=python
BEAM_ROOT_DIR=beam
WEBSITE_ROOT_DIR=beam-site

RC_TAG="v${RELEASE_VERSION}-RC${RC_NUM}"
CURRENT_COMMIT=$(git rev-parse HEAD)

echo "============Tagging ${RC_TAG} = $CURRENT_COMMIT and pushing to GitHub==================="
git tag -a "$RC_TAG" -m "Beam ${RELEASE_VERSION}-${RC_NUM}"

if [[ -z "$NO_SOURCE" ]]; then
  echo "=========Staging Source Release on dist.apache.org==========="
  TEMP_SVN_DIR=$(mktemp -d beam-release-svn-XXXXXXXX)
  SVN_STAGING_DIR="${TEMP_SVN_DIR}/beam/${RELEASE_VERSION}"

  svn checkout "${ROOT_SVN_URL}" "${TEMP_SVN_DIR}"
  mkdir -p "$SVN_STAGING_DIR"

  echo "----------------Downloading Source Release-------------------"
  SOURCE_RELEASE_ZIP="apache-beam-${RELEASE_VERSION}-source-release.zip"
  # Check whether there is an existing dist dir
  if (svn ls "${SVN_STAGING_DIR}/${SOURCE_RELEASE_ZIP}"); then
    echo "Removing existing ${SOURCE_RELEASE_ZIP}."
    svn delete "${SVN_STAGING_DIR}/${SOURCE_RELEASE_ZIP}"
  fi

  echo "Downloading: ${GIT_BEAM_ARCHIVE}/release-${RELEASE}.zip"
  wget ${GIT_BEAM_ARCHIVE}/release-${RELEASE_VERSION}.zip  -O "${SVN_STAGING_URL}/${SOURCE_RELEASE_ZIP}"

  echo "----Signing Source Release ${SOURCE_RELEASE_ZIP}-----"
  (cd ${SVN_STAGING_DIR}
  gpg --local-user ${SIGNING_KEY} --armor --detach-sig "${SOURCE_RELEASE_ZIP}"

  echo "----Creating Hash Value for ${SOURCE_RELEASE_ZIP}----"
  sha512sum ${SOURCE_RELEASE_ZIP} > ${SOURCE_RELEASE_ZIP}.sha512

  # The svn commit is interactive already and can be aborted by deleted the commit msg
  svn add --force .
  svn commit --no-auth-cache -m "Stage apache-beam-${RELEASE_VERSION}-${RC_NUM}")
fi

if [[ -z "$NO_JAVA" ]]; then
  echo "============Building and Staging Java Artifacts============="
  echo "------------Building Java Artifacts with Gradle-------------"
  git config credential.helper store

  echo "-------------Staging Java Artifacts into Maven---------------"
  # Get gpg agent to have the needed key
  gpg --local-user ${SIGNING_KEY} --output /dev/null --sign ~/.bashrc
  ./gradlew publish -Psigning.gnupg.keyName=${SIGNING_KEY} -PisRelease --no-daemon
  echo "You now must log in to Nexus at https://repository.apache.org and 'close' the staging repo named orgapachebeam-NNNN."
  echo "Then please review all artifacts in staging URL. e.g. https://repository.apache.org/content/repositories/orgapachebeam-NNNN/"
fi

if [[ -z "$NO_PYTHON" ]]; then
  echo "============Staging Python Artifacts on dist.apache.org========="
  echo '-------------------Generating Python Artifacts-----------------'
  (cd sdks/python
  virtualenv ${LOCAL_PYTHON_VIRTUALENV}
  source ${LOCAL_PYTHON_VIRTUALENV}/bin/activate
  python setup.py sdist --format=zip
  cd dist

  TEMP_SVN_DIR=$(mktemp -d beam-release-svn-XXXXXXXX)
  svn checkout "${ROOT_SVN_URL}" "${TEMP_SVN_DIR}"
  (cd $TEMP_SVN_DIR
  mkdir -p beam/${RELEASE}/${PYTHON_ARTIFACTS_DIR}
  cp apache-beam-${RELEASE}.zip beam/${RELEASE}/${PYTHON_ARTIFACTS_DIR}/apache-beam-${RELEASE}.zip
  cd beam/${RELEASE}/${PYTHON_ARTIFACTS_DIR}

  echo "------Signing Source Release apache-beam-${RELEASE}.zip------"
  gpg --local-user ${SIGNING_KEY} --armor --detach-sig apache-beam-${RELEASE}.zip

  echo "------Creating Hash Value for apache-beam-${RELEASE}.zip------"
  sha512sum apache-beam-${RELEASE}.zip > apache-beam-${RELEASE}.zip.sha512

  cd ..
  svn add --force ${PYTHON_ARTIFACTS_DIR}
  svn status
  echo "Please confirm these changes are ready to commit: [y|N] "
  read confirmation
  if [[ $confirmation != "y" ]]; then
    echo "Exit without staging python artifacts on dist.apache.org."
    rm -rf ~/${PYTHON_ARTIFACTS_DIR}
    exit
  fi
  svn commit --no-auth-cache
  rm -rf ~/${PYTHON_ARTIFACTS_DIR}
fi

echo "[Current Step]: Update beam-site"
echo "Do you want to proceed? [y|N]"
read confirmation
if [[ $confirmation = "y" ]]; then
  echo "==============Creating PR for Updating Website==============="
USER_WEBSITE_URL=git@github.com:${DOC_PR_USER}/beam-site

  cd ~
  if [[ -d ${LOCAL_WEBSITE_UPDATE_DIR} ]]; then
    rm -rf ${LOCAL_WEBSITE_UPDATE_DIR}
  fi
  mkdir -p ${LOCAL_WEBSITE_UPDATE_DIR}
  cd ${LOCAL_WEBSITE_UPDATE_DIR}
  mkdir -p ${LOCAL_PYTHON_DOC}
  mkdir -p ${LOCAL_JAVA_DOC}
  mkdir -p ${LOCAL_WEBSITE_REPO}

  echo "------------------Building Python Doc------------------------"
  virtualenv ${LOCAL_PYTHON_VIRTUALENV}
  source ${LOCAL_PYTHON_VIRTUALENV}/bin/activate
  cd ${LOCAL_PYTHON_DOC}
  pip install tox
  git clone ${GIT_REPO_URL}
  cd ${BEAM_ROOT_DIR}
  git checkout ${RELEASE_BRANCH}
  cd sdks/python && tox -e docs
  GENERATED_PYDOC=~/${LOCAL_WEBSITE_UPDATE_DIR}/${LOCAL_PYTHON_DOC}/${BEAM_ROOT_DIR}/sdks/python/target/docs/_build
  rm -rf ${GENERATED_PYDOC}/.doctrees

  echo "----------------------Building Java Doc----------------------"
  cd ~/${LOCAL_WEBSITE_UPDATE_DIR}/${LOCAL_JAVA_DOC}
  git clone ${GIT_REPO_URL}
  cd ${BEAM_ROOT_DIR}
  git checkout ${RELEASE_BRANCH}
  ./gradlew :beam-sdks-java-javadoc:aggregateJavadoc
  GENERATE_JAVADOC=~/${LOCAL_WEBSITE_UPDATE_DIR}/${LOCAL_JAVA_DOC}/${BEAM_ROOT_DIR}/sdks/java/javadoc/build/docs/javadoc/

  echo "------------------Updating Release Docs---------------------"
  cd ~/${LOCAL_WEBSITE_UPDATE_DIR}/${LOCAL_WEBSITE_REPO}
  git clone ${GIT_BEAM_WEBSITE}
  cd ${WEBSITE_ROOT_DIR}
  git checkout release-docs
  git checkout -b updates_release_${RELEASE} release-docs

  echo "..........Copying generated javadoc into beam-site.........."
  cp -r ${GENERATE_JAVADOC} javadoc/${RELEASE}

  echo "............Copying generated pydoc into beam-site.........."
  cp -r ${GENERATED_PYDOC} pydoc/${RELEASE}

  git add -A
  git commit -m "Update beam-site for release ${RELEASE}\n\nContent generated based on commit ${RELEASE_COMMIT}"
  git push -f ${USER_REMOTE_URL}

  if [[ -z `which hub` ]]; then
    echo "You don't have hub installed, do you want to install hub with sudo permission? [y|N]"
    read confirmation
    if [[ $confirmation = "y" ]]; then
      HUB_VERSION=2.5.0
      HUB_ARTIFACTS_NAME=hub-linux-amd64-${HUB_VERSION}
      wget https://github.com/github/hub/releases/download/v${HUB_VERSION}/${HUB_ARTIFACTS_NAME}.tgz
      tar zvxvf ${HUB_ARTIFACTS_NAME}.tgz
      sudo ./${HUB_ARTIFACTS_NAME}/install
      rm -rf ${HUB_ARTIFACTS_NAME}*
    fi
  fi
  if [[ -z `which hub` ]]; then
    hub pull-request -m "Publish ${RELEASE} release" -h ${USER_GITHUB_ID}:updates_release_${RELEASE} -b apache:release-docs
  else
    echo "Without hub, you need to create PR manually."
  fi

  echo "Finished v${RELEASE}-RC${RC_NUM} creation."
  rm -rf ~/${LOCAL_WEBSITE_UPDATE_DIR}/${LOCAL_JAVA_DOC}
  rm -rf ~/${LOCAL_WEBSITE_UPDATE_DIR}/${LOCAL_PYTHON_DOC}
fi

echo "===========Please Review All Items in the Checklist=========="
echo "1. Maven artifacts deployed to https://repository.apache.org/content/repositories/"
echo "2. Source distribution deployed to https://dist.apache.org/repos/dist/dev/beam/${RELEASE}"
echo "3. Website pull request published the Java API reference manual the Python API reference manual."

echo "==============Things Needed To Be Done Manually=============="
echo "1.You need to update website updates PR with a new commit: "
echo "  - cd ~/${LOCAL_WEBSITE_UPDATE_DIR}/${LOCAL_WEBSITE_REPO}/${WEBSITE_ROOT_DIR}"
echo "  - git checkout updates_release_${RELEASE}"
echo "  - Add new release into src/get-started/downloads.md "
echo "  - commit your changes"
echo "2.You need to update website updates PR with another commit: src/get-started/downloads.md"
echo "  - add new release download links like commit: "
echo "    https://github.com/apache/beam-site/commit/29394625ce54f0c5584c3db730d3eb6bf365a80c#diff-abdcc989e94369c2324cf64b66659eda"
echo "  - update last release download links from release to archive like commit: "
echo "    https://github.com/apache/beam-site/commit/6b9bdb31324d5c0250a79224507da0ea7ae8ccbf#diff-abdcc989e94369c2324cf64b66659eda"
echo "3.Start the review-and-vote thread on the dev@ mailing list."
