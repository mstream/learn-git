const BrowserFS = require("browserfs");
const git = require("isomorphic-git");

const encoding = "utf8";

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
    fs.readFile(path, encoding, (err, fileNames) =>
      err ? reject(err) : resolve(fileNames)));

  const writeFile = (path, data) => new Promise((resolve, reject) =>
    fs.writeFile(path, data, encoding, (err, fileNames) =>
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

exports.mkGitInitPromise = (path) => () =>
  fsPromise
  .then(({
    fs
  }) => git.init({
    dir: path,
    fs,
  }));
