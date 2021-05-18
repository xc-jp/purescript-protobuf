"use strict";

// // https://github.com/sindresorhus/get-stdin/blob/147d91ccdb23748b4655192781928df9f4fb4aee/index.js#L19-L33
// async function getStdinBufferImpl_() {
// //export async function getStdinBufferImpl() {
// //exports.getStdinBufferImpl = async () => {
// 	const result = [];
// 	let length = 0;
//
// 	if (process.stdin.isTTY) {
// 		return Buffer.concat([]);
// 	}
//
// 	for await (const chunk of process.stdin) {
// 		result.push(chunk);
// 		length += chunk.length;
// 	}
//
// 	return Buffer.concat(result, length);
// }
//
// exports.getStdinBufferImpl = () => { return getStdinBufferImpl_(); }


// https://humanwhocodes.com/snippets/2019/05/nodejs-read-stream-promise/
exports.getStdinBufferImpl = () => {
	// process.stdin.resume();
	return new Promise((resolve, reject) => {
		// console.log("Start the promise");
		let result = [];
		// https://nodejs.org/api/stream.html#stream_event_data
		// process.stdin.on("readable", () => process.stdin.read());
		// process.stdin.on("readable", () => console.log(process.stdin.read()));
		process.stdin.on("data", chunk => result.push(chunk));
		// process.stdin.on("data", chunk => { console.log(chunk); result.push(chunk) });
		process.stdin.on("end", () => resolve(Buffer.concat(result)));
		process.stdin.on("error", error => reject(error));
  });
}