loadModule("/js/management.mjs").addEventListener("load", _ => {
    (async function() {
        function pullImage() {
            let elem = dockerCard.popup.containerPrompt(`Pulling Image`);
            popup.events.addListener("submit", async function(_event) {
                if(!elem.reportValidity()){
                    this.ignoreExecution();
                    return;
                }
                let data = elem.toData();
                
                let resp = await docker.createContainer(data.containerName, slicedImageId, data.command, {ExposedPorts: data.ports});
                let jsonPre = `<pre>${JSON.stringify(resp.data, null, 4)}</pre>`.toDOMElement();
                toasts.add(`Creating ${slicedImageId}: ${resp.statusText}`, jsonPre, {severity: resp.ok ? toasts.severities.INFO : toasts.severities.ERROR});
                if(!resp.ok)
                    this.ignoreExecution();
                else
                    popup.hide();
            }, {executions: 1});
        }
        const buttonPullImage = document.getElementById("button-pull");
        buttonPullImage.addEventListener("click", (_) => pullImage());
    });
    (async function() {
        function createContainer(imageObj) {
            let imageId = imageObj["Id"].split(":")[1];
            let slicedImageId = imageId.slice(0, 12);
            let elem = dockerCard.popup.containerPrompt(`Creating a container of ${slicedImageId}`);
            popup.events.addListener("submit", async function(_event) {
                if(!elem.reportValidity()){
                    this.ignoreExecution();
                    return;
                }
                let data = elem.toData();
                
                let resp = await docker.createContainer(data.containerName, slicedImageId, data.command, {ExposedPorts: data.ports});
                let jsonPre = `<pre>${JSON.stringify(resp.data, null, 4)}</pre>`.toDOMElement();
                toasts.add(`Creating ${slicedImageId}: ${resp.statusText}`, jsonPre, {severity: resp.ok ? toasts.severities.INFO : toasts.severities.ERROR});
                if(!resp.ok)
                    this.ignoreExecution();
                else
                    popup.hide();
            }, {executions: 1});
        }
        function startContainer(imageObj) {
            let imageId = imageObj["Id"].split(":")[1];
            let slicedImageId = imageId.slice(0, 12);
            let elem = dockerCard.popup.containerPrompt(`Starting a container of ${slicedImageId}`);
            popup.events.addListener("submit", async function(_event) {
                if(!elem.reportValidity()){
                    this.ignoreExecution();
                    return;
                }
                let data = elem.toData();
                
                let createResp = await docker.createContainer(data.containerName, slicedImageId, data.command, {ExposedPorts: data.ports});
                toasts.add(`Creating ${slicedImageId}: ${createResp.statusText}`, `<pre>${JSON.stringify(createResp.data, null, 4)}</pre>`.toDOMElement(), {severity: createResp.ok ? toasts.severities.INFO : toasts.severities.ERROR});
                if(!createResp.ok){
                    this.ignoreExecution();
                    return;
                }
                let startResp = await docker.startContainer(createResp.data.Id);
                let toastBody = startResp.data === null ? "" : `<pre>${JSON.stringify(startResp.data, null, 4)}</pre>`.toDOMElement();
                toasts.add(`Starting ${slicedImageId}: ${startResp.statusText}`, toastBody, {severity: createResp.ok ? toasts.severities.INFO : toasts.severities.ERROR});
                if(!startResp.ok){
                    this.ignoreExecution();
                    return;
                }
                popup.hide();
            }, {executions: 1});
        }

        async function getOpts() {
            let fetched = await fetch("/api/socket/direct/images/json");
            if(!fetched.ok)
                return;
            let json = await fetched.text();
            let objects = JSON.parse(json);
            return objects.map(obj => mapOpt(obj));
        }

        function mapOpt(obj) {
            function defaultAction(option) {
                console.log("%O was chosen!", option);
            }
            let imageId = obj["Id"].split(":")[1];
            let id = imageId.slice(0, 8);
            return {
                id: "image" + id,
                title: obj["RepoTags"][0],
                subtitle: id,
                imageId: imageId,
                data: [
                    {name: "Created", value: new Date(obj["Created"]*1000).toISOString()},
                    {name: "Size", value: obj["Size"]},
                    {name: "RepoTags", value: obj["RepoTags"]},
                    {name: "RepoDigests", value: obj["RepoDigests"]}
                ],
                actions: [
                    {value: "Inspect", func: () => dockerCard.popup.inspect(imageId)},
                    {value: "Create Container", func: () => createContainer(obj)},
                    {value: "Start Container", func: () => startContainer(obj)}
                ]
            }
        }
        
        async function refreshImages() {
            let opts = await getOpts();
            for(let i = 0; i < opts.length; i++) {
                const opt = opts[i];
                let card = document.getElementById(opt.id);
                if(card === null){
                    card = dockerCard.create(opt);
                    card.id = opt.id;
                    card.addEventListener("refresh", async (event) => {
                        const promise = event.detail.promise;
                        let url = "/api/image/" + opt.imageId;
                        let fetched = await fetch(url);
                        if(!fetched.ok) {
                            promise.reject(`Failed to GET ${url}`);
                            return;
                        }
                        let json = await fetched.text();
                        let obj = JSON.parse(json);
                        // For whatever reason, inspecting an image gives us ISO8601 time
                        //  rather than milliseconds since creation, so we have to massage
                        //  the data.
                        obj["Created"] = Math.floor(new Date(obj["Created"]).getTime()/1000);
                        let mappedOpt = mapOpt(obj);
                        promise.resolve();
                        if(promise.isResolved()) {
                            card.setTitle(mappedOpt.title);
                            card.setSubtitle(mappedOpt.subtitle);
                            card.updateData(mappedOpt.data);
                        }
                    });
                    card.addEventListener("remove", event => console.log("Attempting to remove %O", card));
                    document.getElementById("image_cards").children[0].appendChild(card);
                } else {
                    card.setTitle(opt.title);
                    card.setSubtitle(opt.subtitle);
                    card.updateData(opt.data);
                }
            }
        };
        refreshImages();
    })();

    (async function() {
        async function getOpts() {
            let fetched = await fetch("/api/socket/direct/containers/json");
            if(!fetched.ok)
                return;
            let json = await fetched.text();
            let objects = JSON.parse(json);
            function defaultAction(option) {
                console.log("%O was chosen!", option);
            }
            return objects.map(obj => {
                let id = obj["Id"].slice(0, 8);
                let imageId = obj["ImageID"].split(":")[1].slice(0, 8);
                return {
                    id: "container" + id,
                    title: id,
                    subtitle: obj["Names"].join(", "),
                    status: obj["State"],
                    data: [
                        {name: "Created", value: new Date(obj["Created"]*1000).toISOString()},
                        {name: "Image", value: obj["Image"]},
                        {name: "ImageId", value: imageId},
                        {name: "Command", value: obj["Command"]},
                        {name: "Names", value: obj["Names"].join(", ")},
                        {name: "Ports", value: obj["Ports"].map(o => `${o["PrivatePort"]}:${o["PublicPort"]}/${o["Type"]}`).join(", ")},
                        {name: "Mounts", value: obj["Mounts"].map(o => `${o["Source"]}:${o["Destination"]}${o["Mode"] !== "" ? ":" + o["Mode"] : ""},${o["Type"]}`).join(", ")},
                        {name: "Status", value: obj["Status"]}
                    ],
                    actions: [
                        {value: "shell", func: defaultAction}
                    ]
                }
            })
        }
        
        async function refreshContainers() {
            let opts = await getOpts();
            for(let i = 0; i < opts.length; i++) {
                const opt = opts[i];
                let card = document.getElementById(opt.id);
                if(card === null){
                    card = dockerCard.create(opt);
                    card.id = opt.id;
                    card.addEventListener("refresh", event => console.log("Attempting to refresh %O", card))
                    card.addEventListener("remove", event => console.log("Attempting to remove %O", card))
                    document.getElementById("container_cards").children[0].appendChild(card);
                } else {
                    card.setTitle(opt.title);
                    card.setSubtitle(opt.subtitle);
                    card.updateData(opt.data);
                }
            }
        };
        refreshContainers();
    })();
});