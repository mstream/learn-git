const filer = require("filer");

const path = filer.Path

const fs = new filer.FileSystem({
  name: "in-memory-fs",
  flags: ["FORMAT"],
  provider: new filer.FileSystem.providers.Memory()
}).promises;

const encoding = "utf8";

const reportError = name => err => console.error("error in", name, err);

const reportValue = name => val => {
  console.debug("executing", name, "with result", val);
  return new Promise(resolve => resolve(val));
};

exports.joinPaths = path1 => path2 => path.join(path1, path2)

exports.mkExistsPromise = (path) => () =>
  fs.exists(path).then(reportValue("exists")).catch(reportError("exists"));

exports.mkReadDirPromise = (path) => () =>
  fs.readdir(path).then(reportValue("readDir")).catch(reportError("readDir"));

exports.mkReadFilePromise = (path) => () =>
  fs.readFile(path, {
    encoding
  }).then(reportValue("readFile")).catch(reportError("readFile"));

exports.mkMkDirPromise = (path) => () => {
  console.log(path)
  return fs.mkdir(path).then(reportValue("mkDir")).catch(reportError("mkDir"));
}

exports.mkWriteFilePromise = (path) => (data) => () => {
  console.log(path)
  return fs.writeFile(path, data).then(reportValue("writeFile")).catch(reportError("writeFile"));
}

exports.mkIsDirPromise = (path) => () =>
  fs.lstat(path)
  .then(reportValue("lstat"))
  .then(stats => stats.isDirectory())
  .then(reportValue("isDirectory"))
  .catch(reportError("isDir"));
