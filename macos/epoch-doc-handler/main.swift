import AppKit
import Carbon.HIToolbox

enum HandlerError: LocalizedError {
    case missingURL
    case helperFailed(String)
    case launchFailed(String)

    var errorDescription: String? {
        switch self {
        case .missingURL:
            return "LaunchServices did not provide a document URL."
        case .helperFailed(let message), .launchFailed(let message):
            return message
        }
    }
}

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var handlingRequest = false

    func applicationWillFinishLaunching(_ notification: Notification) {
        NSAppleEventManager.shared().setEventHandler(
            self,
            andSelector: #selector(handleGetURL(_:withReplyEvent:)),
            forEventClass: AEEventClass(kInternetEventClass),
            andEventID: AEEventID(kAEGetURL)
        )
    }

    @objc private func handleGetURL(
        _ event: NSAppleEventDescriptor,
        withReplyEvent replyEvent: NSAppleEventDescriptor
    ) {
        guard !handlingRequest else { return }
        handlingRequest = true

        guard let rawURL = event
            .paramDescriptor(forKeyword: AEKeyword(keyDirectObject))?
            .stringValue
        else {
            showError(HandlerError.missingURL.localizedDescription)
            return
        }

        do {
            try runHelper(rawURL)
            NSApp.terminate(nil)
        } catch {
            showError(error.localizedDescription)
        }
    }

    private func runHelper(_ rawURL: String) throws {
        let helper = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent("bin/epoch-doc-link")
        let process = Process()
        let stderr = Pipe()
        process.executableURL = helper
        process.arguments = ["open", rawURL]
        process.standardError = stderr

        do {
            try process.run()
        } catch {
            throw HandlerError.launchFailed(
                "Could not launch \(helper.path): \(error.localizedDescription)"
            )
        }

        let data = stderr.fileHandleForReading.readDataToEndOfFile()
        process.waitUntilExit()
        guard process.terminationStatus == 0 else {
            let detail = String(data: data, encoding: .utf8)?
                .trimmingCharacters(in: .whitespacesAndNewlines)
            throw HandlerError.helperFailed(
                detail?.isEmpty == false
                    ? detail!
                    : "epoch-doc-link exited with status \(process.terminationStatus)."
            )
        }
    }

    private func showError(_ message: String) {
        NSApp.activate(ignoringOtherApps: true)
        let alert = NSAlert()
        alert.alertStyle = .critical
        alert.messageText = "Epoch document link could not be opened"
        alert.informativeText = message
        alert.runModal()
        NSApp.terminate(nil)
    }

    deinit {
        NSAppleEventManager.shared().removeEventHandler(
            forEventClass: AEEventClass(kInternetEventClass),
            andEventID: AEEventID(kAEGetURL)
        )
    }
}

@main
struct EpochDocumentHandler {
    static func main() {
        let application = NSApplication.shared
        let delegate = AppDelegate()
        application.setActivationPolicy(.accessory)
        application.delegate = delegate
        application.run()
    }
}
