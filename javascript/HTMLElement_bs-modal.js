// <bs-modal id="myId" title="Title" message="Message" button="OK" class="h3 text-danger" size="lg" onhide="console.log('myID')"></bs-modal>

class ElementBsModal extends HTMLElement {
    constructor() {
	super()
    }

    static get observedAttributes() { return ["title", "message", "button", "size", "class", "onhide"] }

    attributeChangedCallback(name, oldValue, newValue) {
	//console.log("attributeChangedCallback(name='" + name + "' + oldValue='" + oldValue + "' + newValue='" + newValue + "'");
	switch (name) {
	    case "title":
		break;
	}
    }
    connectedCallback() {
	//console.log("connectedCallback()");
    }
    disconnectedCallback() {
	//console.log("disconnectedCallback()");
    }
    adoptedCallback() {
	//console.log("adoptedCallback()");
    }

    get title() {
	//console.log("title.get()");
	return this.getAttribute("title");
    }
    set title(newValue) {
	//console.log("title.set('" + newValue + "')");
	this.setAttribute('title', newValue);
    }

    get message() {
	return this.getAttribute("message");
    }
    set message(newValue) {
	this.setAttribute('message', newValue);
    }

    get button() {
	return this.getAttribute("button");
    }
    set button(newValue) {
	this.setAttribute('button', newValue);
    }

    get size() {
	return this.getAttribute("size");
    }
    set size(newValue) {
	this.setAttribute('size', newValue);
    }

    get class() {
	return this.getAttribute("class");
    }
    set class(newValue) {
	this.setAttribute('class', newValue);
    }

    get onhide() {
	return this.getAttribute("onhide");
    }
    set onhide(newValue) {
	this.setAttribute('onhide', newValue);
    }
}
window.customElements.define('bs-modal', ElementBsModal);
