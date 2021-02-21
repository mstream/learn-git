const BrowserFS = require("browserfs");
const git = require("isomorphic-git");

const isBinary = (buffer) => {
  if (buffer.length === 0) {
    return false;
  }

  let suspiciousBytes = 0;

  const totalBytes = Math.min(buffer.length, 1024);

  if (
    buffer.length >= 3 &&
    fileBuffer[0] === 0xef &&
    fileBuffer[1] === 0xbb &&
    fileBuffer[2] === 0xbf
  ) {
    return false;
  }

  if (
    buffer.length >= 4 &&
    fileBuffer[0] === 0x00 &&
    fileBuffer[1] === 0x00 &&
    fileBuffer[2] === 0xfe &&
    fileBuffer[3] === 0xff
  ) {
    return false;
  }

  if (
    buffer.length >= 4 &&
    fileBuffer[0] === 0xff &&
    fileBuffer[1] === 0xfe &&
    fileBuffer[2] === 0x00 &&
    fileBuffer[3] === 0x00
  ) {
    return false;
  }

  if (
    buffer.length >= 4 &&
    fileBuffer[0] === 0x84 &&
    fileBuffer[1] === 0x31 &&
    fileBuffer[2] === 0x95 &&
    fileBuffer[3] === 0x33
  ) {
    return false;
  }

  if (totalBytes >= 5 && fileBuffer.slice(0, 5).toString() === '%PDF-') {
    return true;
  }

  if (buffer.length >= 2 && fileBuffer[0] === 0xfe && fileBuffer[1] === 0xff) {
    return false;
  }

  if (buffer.length >= 2 && fileBuffer[0] === 0xff && fileBuffer[1] === 0xfe) {
    return false;
  }

  for (let i = 0; i < totalBytes; i++) {
    if (fileBuffer[i] === 0) {
      return true;
    } else if ((fileBuffer[i] < 7 || fileBuffer[i] > 14) && (fileBuffer[i] < 32 || fileBuffer[i] > 127)) {
      if (fileBuffer[i] > 193 && fileBuffer[i] < 224 && i + 1 < totalBytes) {
        i++;
        if (fileBuffer[i] > 127 && fileBuffer[i] < 192) {
          continue;
        }
      } else if (fileBuffer[i] > 223 && fileBuffer[i] < 240 && i + 2 < totalBytes) {
        i++;
        if (fileBuffer[i] > 127 && fileBuffer[i] < 192 && fileBuffer[i + 1] > 127 && fileBuffer[i + 1] < 192) {
          i++;
          continue;
        }
      }

      suspiciousBytes++;

      if (i > 32 && (suspiciousBytes * 100) / totalBytes > 10) {
        return true;
      }
    }
  }

  if ((suspiciousBytes * 100) / totalBytes > 10) {
    return true;
  }

  return false;
}


const fsPromise = new Promise((resolve, reject) => {
  BrowserFS.configure({
    fs: "InMemory",
  }, (err) => err ? reject(err) : resolve());
  setTimeout(reject, 1000);
}).then(() => {
  const fs = BrowserFS.BFSRequire("fs");

  const exists = path => new Promise(resolve =>
    fs.exists(path, resolve));

  const lstat =
    path => new Promise((resolve, reject) =>
      fs.lstat(path, (err, stats) =>
        err ? reject(err) : resolve(stats)));

  const mkdir =
    path => new Promise(
      (resolve, reject) =>
      fs.mkdir(path, err => err ? reject(e) : resolve()));

  const readdir = path => new Promise((resolve, reject) =>
    fs.readdir(path, (err, fileNames) =>
      err ? reject(err) : resolve(fileNames)));

  const readFile = path => new Promise((resolve, reject) =>
    fs.readFile(path, (err, fileNames) =>
      err ? reject(err) : resolve(fileNames)));

  const writeFile = (path, data) => new Promise((resolve, reject) =>
    fs.writeFile(path, data, "utf8", (err, fileNames) =>
      err ? reject(err) : resolve(fileNames)));

  return {
    exists,
    fs,
    lstat,
    mkdir,
    readdir,
    readFile,
    writeFile,
  };
});

exports.isBinary = isBinary;

exports.joinPaths = path1 => path2 => path.join(path1, path2)

exports.mkExistsPromise = (path) => () =>
  fsPromise.then(({
    exists
  }) => exists(path));

exports.mkReadDirPromise = (path) => () =>
  fsPromise
  .then(({
    readdir
  }) => readdir(path));

exports.mkReadFilePromise = (path) => () =>
  fsPromise
  .then(({
    readFile
  }) => readFile(path));


exports.mkMkDirPromise = (path) => () =>
  fsPromise
  .then(({
    mkdir
  }) => mkdir(path));

exports.mkWriteFilePromise = (path) => (data) => () =>
  fsPromise
  .then(({
    writeFile
  }) => writeFile(path, data));

exports.mkIsDirPromise =
  (path) => () =>
  fsPromise.then(({
    lstat
  }) => lstat(path))
  .then(stats => stats.isDirectory());

exports.mkGitAddPromise = (repoDirPath) => (pathSpec) => () =>
  fsPromise
  .then(({
    fs
  }) => git.add({
    dir: repoDirPath,
    filepath: pathSpec,
    fs,
  }));

exports.mkGitCommitPromise = (repoDirPath) => (msg) => () =>
  fsPromise
  .then(({
    fs
  }) => git.commit({
    author: {
      name: "DUMMY",
      email: "DUMMY@DUMMY.DUMMY",
    },
    dir: repoDirPath,
    fs,
    message: msg,
  }));

exports.mkGitInitPromise = (repoDirPath) => () =>
  fsPromise
  .then(({
    fs
  }) => git.init({
    dir: repoDirPath,
    fs,
  }));
