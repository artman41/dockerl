export class Docker {
    constructor() {
        this.fetch = async function(url, opts) {
            console.log("Fetching '%O' with opts %O", url, opts);
            let resp = await window.fetch(url, opts);
            let text = await resp.text();
            return {
                ok: resp.ok,
                status: resp.status,
                statusText: resp.statusText,
                headers: resp.headers,
                data: text.length === 0 ? null : JSON.parse(text),
            };
        }
    }

    getImage(imageId) {
        return this.fetch("/api/image/" + imageId);
    }

    getContainer(containerName) {
        if(containerName === undefined)
            return new Promise((_, reject) => reject(new Error("Invalid arguments provided! " + arguments.toNiceString())) );
        return this.fetch(`/api/socket/direct/containers/${containerName}/json`)
    }

    createContainer(containerName, imageName, command, opts) {
        opts = opts ?? {};
        if([containerName, imageName, command].any(elem => elem === undefined) || !/\/?[a-zA-Z0-9][a-zA-Z0-9_.-]+/.test(containerName))
            return new Promise((_, reject) => reject(new Error("Invalid arguments provided! " + arguments.toNiceString())) );
        console.log("containerName: %O", containerName);
        const keys = [
            'Hostname', 
            'Domainname', 
            'User', 
            'AttachStdin', 
            'AttachStdout', 
            'AttachStderr', 
            'ExposedPorts', 
            'Tty', 
            'OpenStdin', 
            'StdinOnce', 
            'Env', 
            'Healthcheck', 
            'ArgsEscaped', 
            'Volumes', 
            'WorkingDir', 
            'Entrypoint', 
            'NetworkDisabled', 
            'MacAddress', 
            'OnBuild', 
            'Labels', 
            'StopSignal', 
            'StopTimeout', 
            'Shell', 
            'HostConfig', 
            'NetworkingConfig'
        ];
        let body = {
            Cmd: command,
            Image: imageName
        };
        keys.forEach(key => {
            if(opts[key] !== undefined) 
                body[key] = opts[key];
        });
        return this.fetch(`/api/socket/direct/containers/create?name=${containerName}`, {
            method: 'POST',
            headers: {
                "Content-Type": "application/json"
            },
            body: JSON.stringify(body),
        })
    }

    startContainer(containerName, opts) {
        opts = opts ?? {};
        if(containerName === undefined)
            return new Promise((_, reject) => reject(new Error("Invalid arguments provided! " + arguments.toNiceString())) );
        let queryParams = opts["detachKeys"] === undefined ? "" : "detachKeys=" + opts["detachKeys"];
        return this.fetch(`/api/socket/direct/containers/${containerName}/start?${queryParams}`, {
            method: 'POST'
        })
    }

    stopContainer(containerName, opts) {
        opts = opts ?? {};
        if(containerName === undefined)
            return new Promise((_, reject) => reject(new Error("Invalid arguments provided! " + arguments.toNiceString())) );
        let queryParams = opts["t"] === undefined ? "" : "t=" + opts["t"];
        return this.fetch(`/api/socket/direct/containers/${containerName}/stop?${queryParams}`, {
            method: 'POST'
        })
    }
}

window.docker = new Docker();