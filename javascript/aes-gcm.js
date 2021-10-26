(async () => {
    const txtEnc = new TextEncoder();
    const txtDec = new TextDecoder();
    const password = '5ecure/Passw0rd';
    const toEncrypt = 'Hello World';

    // escape -> encodeURI, unescape -> decodeURI

    console.log('password:', password);
    console.log('to encrypt:', toEncrypt);

    binaryToUint8Array = function (data) {
        return new Uint8Array(data.split('').map(function (c) { return c.charCodeAt(0); }));
    }

    function byteArrayBase64urlEncode(arr) {
        const b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        const bin = n => n.toString(2).padStart(8, 0);
        const l = arr.length
        let result = '';

        for (let i = 0; i <= (l - 1) / 3; i++) {
            let c1 = i * 3 + 1 >= l;
            let c2 = i * 3 + 2 >= l;
            let chunk = bin(arr[3 * i]) + bin(c1 ? 0 : arr[3 * i + 1]) + bin(c2 ? 0 : arr[3 * i + 2]);
            let r = chunk.match(/.{1,6}/g).map((x, j) => j == 3 && c2 ? '=' : (j == 2 && c1 ? '=' : b64[+('0b' + x)]));
            result += r.join('');
        }

        return result.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '');
    }

    function base64urlDecode(b64url) {
        return atob((b64url + '==='.slice((b64url.length + 3) % 4)).replace(/-/g, '+').replace(/_/g, '/'))
    }

    function base64urlEncode(data) {
        return btoa(data).replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '')
    }

    function validateKey(str) {
        return str.slice(0, 32) + '\0'.repeat(32 - str.length);
    }


    let key = base64urlEncode(validateKey(password));

    const aesGcmKey = await window.crypto.subtle.importKey(
        'jwk',
        {
            kty: 'oct',
            k: key,
            alg: 'A256GCM',
            ext: true
        },
        { name: 'AES-GCM' },
        false,
        ['encrypt', 'decrypt']
    );

    const iv = crypto.getRandomValues(new Uint8Array(10));

    const encryptedMessage = await crypto.subtle.encrypt(
        { name: 'AES-GCM', iv },
        aesGcmKey,
        txtEnc.encode(toEncrypt)
    );
    console.log('encrypted data:', new Uint8Array(encryptedMessage));

    console.log('base64url encode + decode');
    let b64u = byteArrayBase64urlEncode(new Uint8Array(encryptedMessage));
    let ui8a = binaryToUint8Array(base64urlDecode(b64u));
    console.log('encrypted data:', ui8a);

    const decryptedBuffer = await window.crypto.subtle.decrypt(
        { name: 'AES-GCM', iv: iv },
        aesGcmKey,
        //encryptedMessage
        ui8a
    );
    const decryptedMessage = txtDec.decode(decryptedBuffer);
    console.log('decrypted data:', decryptedMessage);

    console.log('base64url encoded key:', key);
    console.log('base64url encoded nonce iv:', byteArrayBase64urlEncode(iv));
    console.log('base64url encoded encrypted data:', b64u);

    console.log('done');
})();
